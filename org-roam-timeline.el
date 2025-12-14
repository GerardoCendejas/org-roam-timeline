;;; org-roam-timeline.el --- Timeline with Content Preview & Navigation -*- lexical-binding: t; -*-

(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name)))

;; --- SIGNALS ---
(defvar org-roam-timeline--explicit-focus-id nil)
(defvar org-roam-timeline--explicit-hide-id nil)
(defvar org-roam-timeline--explicit-date-focus nil)
(defvar org-roam-timeline--filter-signal nil)
(defvar org-roam-timeline--toggle-follow-signal nil)
(defvar org-roam-timeline--toggle-preview-signal nil) 

;; --- HELPER: Get All Tags ---
(defun org-roam-timeline--get-all-tags ()
  (mapcar #'car (org-roam-db-query [:select :distinct tag :from tags])))

;; --- CONFIGURATION ---
(defgroup org-roam-timeline nil "Settings for Org Roam Timeline." :group 'org-roam)
(defcustom org-roam-timeline-default-theme 'dark "Default theme." :type '(choice (const :tag "Dark" dark) (const :tag "Light" light)))
(defcustom org-roam-timeline-focus-window-years 5 "Zoom window." :type 'integer)
(defcustom org-roam-timeline-show-links-on-start t "Show lines on start." :type 'boolean)
(defcustom org-roam-timeline-follow-mode-on-start t "Follow mode active on start." :type 'boolean)
(defcustom org-roam-timeline-preview-on-start t "Auto-open preview panel on start." :type 'boolean) 

;; --- DATA PROCESSING ---
(defun org-roam-timeline--process-node (node)
  (condition-case err
      (let* ((props (org-roam-node-properties node))
             (id (org-roam-node-id node))
             (start-raw (cdr (assoc "TIMELINE_START" props)))
             (end-raw   (cdr (assoc "TIMELINE_END" props)))
             (start-clean (org-roam-timeline--clean-date start-raw nil))
             (end-clean   (org-roam-timeline--clean-date end-raw t)))

        (when start-clean
          (let* ((tags (or (org-roam-node-tags node) '("Uncategorized")))
                 (backlinks (org-roam-backlinks-get node))
                 (incoming-ids (mapcar (lambda (bl) (org-roam-node-id (org-roam-backlink-source-node bl))) backlinks))
                 (outgoing-ids (ignore-errors (mapcar #'car (org-roam-db-query [:select dest :from links :where (= source $s1)] id))))
                 (all-neighbors (append incoming-ids outgoing-ids))
                 (backlink-count (length backlinks))
                 (importance-class (cond ((> backlink-count 10) "item-huge") ((> backlink-count 5) "item-large") (t "item-small"))))
            
            `((id . ,id)
              (content . ,(org-roam-node-title node))
              (title . "Metadata")
              (start . ,start-clean)
              ,@(when end-clean `((end . ,end-clean)))
              (type . ,(if end-clean "range" "point"))
              (className . ,importance-class)
              (all_tags . ,tags)
              (neighbors . ,all-neighbors)))))
    (error nil)))

(defun org-roam-timeline--get-nodes ()
  (let ((nodes '()))
    (dolist (node (org-roam-node-list))
      (let ((item (org-roam-timeline--process-node node)))
        (when item (push item nodes))))
    nodes))

(defun org-roam-timeline--clean-date (date-str &optional is-end)
  (when (stringp date-str)
    (let ((clean (string-trim date-str)))
      (cond
       ((string-match "^-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" clean) clean)
       ((string-match "^\\(-?[0-9]\\{4\\}\\)$" clean)
        (if is-end (concat (match-string 1 clean) "-12-31") (concat (match-string 1 clean) "-01-01")))
       ((string-match "[\\[<]\\(-?[0-9]+\\S-*\\)[\\]>]" clean) (match-string 1 clean))
       (t nil)))))

;; --- SERVLETS ---
(defservlet* content text/html (id)
  (require 'ox-html)
  (let ((marker (org-id-find id 'marker)) (final-output ""))
    (if (not marker) (setq final-output (format "<h3>Error: ID %s not found.</h3>" id))
      (let* ((raw-content (with-current-buffer (marker-buffer marker) (save-excursion (goto-char (marker-position marker)) (let ((beg (point)) (end (if (org-at-heading-p) (save-excursion (org-end-of-subtree) (point)) (point-max)))) (buffer-substring-no-properties beg end)))))
             (node (org-roam-node-from-id id)) 
             (title (org-roam-node-title node))
             (backlinks (org-roam-backlinks-get node)))
        (condition-case err
            (let ((org-export-use-babel nil) (org-confirm-babel-evaluate nil) (org-html-with-latex 'mathjax) (org-export-with-section-numbers nil) (org-html-link-org-files-as-html nil)) 
              (let ((html (org-export-string-as raw-content 'html t '(:with-toc nil :with-section-numbers nil))))
                (setq html (concat (format "<h1 class='node-title'>%s</h1>" title) html))
                (let ((bl-html "<div class='backlinks-section' style='margin-top:40px; border-top:1px solid #eee; padding-top:20px;'><h4 style='color:#888; text-transform:uppercase; font-size:12px; letter-spacing:1px;'>Linked References</h4><ul style='list-style:none; padding:0;'>"))
                  (dolist (bl backlinks) (let* ((source-node (org-roam-backlink-source-node bl)) (src-id (org-roam-node-id source-node)) (src-title (org-roam-node-title source-node))) (setq bl-html (concat bl-html (format "<li style='margin-bottom:8px;'><a href='id:%s'>%s</a></li>" src-id src-title)))))
                  (setq bl-html (concat bl-html "</ul></div>")) (setq final-output (concat "<div class='org-content'>" html bl-html "</div>")))))
          (error (setq final-output (format "<h3 style='color:red'>Export Failed</h3><pre>%s</pre>" err))))))
    (insert final-output)))

;; UPDATED CONFIG SERVLET (Sends preview setting)
(defservlet* config text/json ()
  (insert (json-encode `((theme . ,(symbol-name org-roam-timeline-default-theme)) 
                         (showLinks . ,(if org-roam-timeline-show-links-on-start t :json-false)) 
                         (followMode . ,(if org-roam-timeline-follow-mode-on-start t :json-false)) 
                         (autoPreview . ,(if org-roam-timeline-preview-on-start t :json-false)) 
                         (zoomWindow . ,org-roam-timeline-focus-window-years)))))

(defservlet* data text/json () (insert (json-encode (org-roam-timeline--get-nodes))))
(defservlet* node-data text/json (id)
  (let ((node (org-roam-node-from-id id))) (if node (let ((item (org-roam-timeline--process-node node))) (if item (insert (json-encode item)) (insert "{}"))) (insert "{}"))))
(defservlet* open text/plain (id) (let ((node (org-roam-node-from-id id))) (if node (progn (with-current-buffer (window-buffer (selected-window)) (org-roam-node-open node)) (insert "Opened")) (insert "Node not found"))))

;; --- POLLING SERVLET ---
(defservlet* current-focus text/json ()
  (let ((response '((action . "none"))))
    
    ;; 1. Filters
    (when org-roam-timeline--filter-signal
      (setq response `((action . ,(car org-roam-timeline--filter-signal)) 
                       (tag . ,(cdr org-roam-timeline--filter-signal))))
      (setq org-roam-timeline--filter-signal nil))

    ;; 2. Date Zoom
    (unless (assoc 'tag response)
      (when org-roam-timeline--explicit-date-focus
        (setq response `((action . "zoom-date") (date . ,org-roam-timeline--explicit-date-focus)))
        (setq org-roam-timeline--explicit-date-focus nil)))

    ;; 3. Toggle Follow
    (unless (or (assoc 'tag response) (string-equal (cdr (assoc 'action response)) "zoom-date"))
      (when org-roam-timeline--toggle-follow-signal
        (setq response `((action . "toggle-follow")))
        (setq org-roam-timeline--toggle-follow-signal nil)))

    ;; 4. Toggle Preview (NEW)
    (unless (or (assoc 'tag response) 
                (string-equal (cdr (assoc 'action response)) "zoom-date")
                (string-equal (cdr (assoc 'action response)) "toggle-follow"))
      (when org-roam-timeline--toggle-preview-signal
        (setq response `((action . "toggle-preview")))
        (setq org-roam-timeline--toggle-preview-signal nil)))

    ;; 5. Hide
    (unless (or (assoc 'tag response) 
                (string-equal (cdr (assoc 'action response)) "zoom-date")
                (string-equal (cdr (assoc 'action response)) "toggle-follow")
                (string-equal (cdr (assoc 'action response)) "toggle-preview"))
      (when org-roam-timeline--explicit-hide-id
        (setq response `((action . "hide") (id . ,org-roam-timeline--explicit-hide-id)))
        (setq org-roam-timeline--explicit-hide-id nil)))

    ;; 6. Focus / Follow
    (if (string-equal (cdr (assoc 'action response)) "none")
        (if org-roam-timeline--explicit-focus-id
            (progn
              (setq response `((action . "focus") (id . ,org-roam-timeline--explicit-focus-id)))
              (setq org-roam-timeline--explicit-focus-id nil))
          (let* ((user-window (selected-window))
                 (user-buffer (window-buffer user-window))
                 (node-id nil))
            (with-current-buffer user-buffer
              (when (derived-mode-p 'org-mode)
                (let ((node (org-roam-node-at-point)))
                  (when node (setq node-id (org-roam-node-id node))))))
            (when node-id
              (setq response `((action . "follow") (id . ,node-id)))))))
    
    (insert (json-encode response))))

(defservlet* remove-date text/plain (id) (let ((node (org-roam-node-from-id id))) (if node (let ((file (org-roam-node-file node)) (point (org-roam-node-point node))) (with-current-buffer (find-file-noselect file) (goto-char point) (org-delete-property "TIMELINE_START") (org-delete-property "TIMELINE_END") (save-buffer)) (insert "Removed")) (insert "Node not found"))))

;; --- INTERACTIVE COMMANDS ---
(defun org-roam-timeline-open () (interactive) (setq httpd-root (expand-file-name "html" org-roam-timeline-root)) (httpd-start) (browse-url (format "http://localhost:%d" httpd-port)))

(defun org-roam-timeline-show-node () (interactive) (let ((node (org-roam-node-at-point))) (if node (progn (setq org-roam-timeline--explicit-focus-id (org-roam-node-id node)) (message "Timeline: Show Node")) (user-error "No node at point"))))
(defun org-roam-timeline-hide-node () (interactive) (let ((node (org-roam-node-at-point))) (if node (progn (setq org-roam-timeline--explicit-hide-id (org-roam-node-id node)) (message "Timeline: Hide Node")) (user-error "No node at point"))))
(defun org-roam-timeline-toggle-follow () (interactive) (setq org-roam-timeline--toggle-follow-signal t) (message "Timeline: Toggled Follow Mode"))

(defun org-roam-timeline-toggle-preview () 
  "Toggle Auto-Preview (Sidebar content) in the browser."
  (interactive) 
  (setq org-roam-timeline--toggle-preview-signal t) 
  (message "Timeline: Toggled Auto-Preview"))

(defun org-roam-timeline-tag-add () (interactive) (let* ((node (org-roam-node-at-point)) (existing-tags (org-roam-db-query [:select :distinct tag :from tags])) (flat-tags (mapcar #'car existing-tags)) (choice (completing-read "Tag: " flat-tags nil nil nil nil))) (if (string-blank-p choice) (message "No tag.") (org-roam-tag-add (list choice)) (message "Tag '%s' added." choice))))
(defun org-roam-timeline-add-date () (interactive) (let* ((start-input (read-string "Start (YYYY or YYYY-MM-DD): ")) (is-range (y-or-n-p "Is this a time range? ")) (end-input (if is-range (read-string "End (YYYY or YYYY-MM-DD): ") nil))) (unless (string-empty-p start-input) (org-set-property "TIMELINE_START" start-input)) (if end-input (org-set-property "TIMELINE_END" end-input) (org-delete-property "TIMELINE_END")) (save-buffer) (org-roam-db-sync) (org-roam-timeline-show-node)))
(defun org-roam-timeline-zoom-date () (interactive) (let ((date-str (read-string "Zoom to Date (YYYY[-MM[-DD]]): "))) (unless (string-empty-p date-str) (setq org-roam-timeline--explicit-date-focus date-str) (message "Timeline: Zooming to %s..." date-str))))

(defun org-roam-timeline-filter-toggle () (interactive) (let ((tag (completing-read "Toggle Tag: " (org-roam-timeline--get-all-tags) nil t))) (setq org-roam-timeline--filter-signal (cons "filter-toggle" tag)) (message "Timeline: Toggling %s" tag)))
(defun org-roam-timeline-filter-block () (interactive) (let ((tag (completing-read "Block Tag: " (org-roam-timeline--get-all-tags) nil t))) (setq org-roam-timeline--filter-signal (cons "filter-block" tag)) (message "Timeline: Blocking %s" tag)))
(defun org-roam-timeline-filter-reset () (interactive) (setq org-roam-timeline--filter-signal (cons "filter-reset" "all")) (message "Timeline: Resetting filters"))
(defun org-roam-timeline-filter-hide-all () (interactive) (setq org-roam-timeline--filter-signal (cons "filter-hide-all" "all")) (message "Timeline: Hiding all tags (Blank Slate)."))

(provide 'org-roam-timeline)
