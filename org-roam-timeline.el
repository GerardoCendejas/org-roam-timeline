;;; org-roam-timeline.el --- Timeline with Content Preview & Navigation -*- lexical-binding: t; -*-

(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name)))
(defvar org-roam-timeline--explicit-focus-id nil)

;; --- CONFIGURATION ---
(defgroup org-roam-timeline nil "Settings for Org Roam Timeline." :group 'org-roam)
(defcustom org-roam-timeline-default-theme 'dark "Default theme." :type '(choice (const :tag "Dark" dark) (const :tag "Light" light)))
(defcustom org-roam-timeline-show-links-on-start t "Show lines on start." :type 'boolean)
(defcustom org-roam-timeline-follow-mode-on-start t "Follow mode active." :type 'boolean)
(defcustom org-roam-timeline-focus-window-years 5 "Zoom window." :type 'integer)

;; --- DATA PROCESSING (Raw Data Only) ---
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
              ;; CRITICAL FIX: We send a dummy string. 
              ;; Vis.js sees this and decides to show a tooltip.
              ;; Then our JS Template overwrites this string with the pills.
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

(defservlet* config text/json ()
  (insert (json-encode `((theme . ,(symbol-name org-roam-timeline-default-theme)) (showLinks . ,(if org-roam-timeline-show-links-on-start t :json-false)) (followMode . ,(if org-roam-timeline-follow-mode-on-start t :json-false)) (zoomWindow . ,org-roam-timeline-focus-window-years)))))
(defservlet* data text/json () (insert (json-encode (org-roam-timeline--get-nodes))))
(defservlet* node-data text/json (id)
  (let ((node (org-roam-node-from-id id))) (if node (let ((item (org-roam-timeline--process-node node))) (if item (insert (json-encode item)) (insert "{}"))) (insert "{}"))))
(defservlet* open text/plain (id) (let ((node (org-roam-node-from-id id))) (if node (progn (with-current-buffer (window-buffer (selected-window)) (org-roam-node-open node)) (insert "Opened")) (insert "Node not found"))))
(defservlet* current-focus text/plain () (if org-roam-timeline--explicit-focus-id (progn (insert org-roam-timeline--explicit-focus-id) (setq org-roam-timeline--explicit-focus-id nil)) (let* ((user-window (selected-window)) (user-buffer (window-buffer user-window)) (node-id nil)) (with-current-buffer user-buffer (when (derived-mode-p 'org-mode) (let ((node (org-roam-node-at-point))) (when node (setq node-id (org-roam-node-id node)))))) (insert (or node-id "")))))
(defservlet* remove-date text/plain (id) (let ((node (org-roam-node-from-id id))) (if node (let ((file (org-roam-node-file node)) (point (org-roam-node-point node))) (with-current-buffer (find-file-noselect file) (goto-char point) (org-delete-property "TIMELINE_START") (org-delete-property "TIMELINE_END") (save-buffer)) (insert "Removed")) (insert "Node not found"))))

(defun org-roam-timeline-open () (interactive) (setq httpd-root (expand-file-name "html" org-roam-timeline-root)) (httpd-start) (browse-url (format "http://localhost:%d" httpd-port)))
(defun org-roam-timeline-show-node () (interactive) (let ((node (org-roam-node-at-point))) (if node (progn (setq org-roam-timeline--explicit-focus-id (org-roam-node-id node)) (message "Signal sent.")) (user-error "No node at point"))))
(defun org-roam-timeline-tag-add () (interactive) (let* ((node (org-roam-node-at-point)) (existing-tags (org-roam-db-query [:select :distinct tag :from tags])) (flat-tags (mapcar #'car existing-tags)) (choice (completing-read "Tag: " flat-tags nil nil nil nil))) (if (string-blank-p choice) (message "No tag.") (org-roam-tag-add (list choice)) (message "Tag '%s' added." choice))))
(defun org-roam-timeline-add-date () (interactive) (let* ((start-input (read-string "Start: ")) (is-range (y-or-n-p "Range? ")) (end-input (if is-range (read-string "End: ") nil))) (unless (string-empty-p start-input) (org-set-property "TIMELINE_START" start-input)) (if end-input (org-set-property "TIMELINE_END" end-input) (org-delete-property "TIMELINE_END")) (save-buffer) (org-roam-db-sync) (org-roam-timeline-show-node)))

(provide 'org-roam-timeline)
