;;; org-roam-timeline.el --- Timeline with Content Preview & Navigation -*- lexical-binding: t; -*-

(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name)))
(defvar org-roam-timeline--explicit-focus-id nil
  "Stores an ID that the user explicitly requested to show via M-x command.")

;; --- 1. Helpers ---
(defun org-roam-timeline-add-date ()
  "Interactively add timeline properties."
  (interactive)
  (let* ((start-input (read-string "Start Date (YYYY-MM-DD, YYYY, or -YYYY): "))
         (is-range (y-or-n-p "Is this a time range (period)? "))
         (end-input (if is-range (read-string "End Date: ") nil)))
    (unless (string-empty-p start-input)
      (org-set-property "TIMELINE_START" start-input))
    (if (and end-input (not (string-empty-p end-input)))
        (org-set-property "TIMELINE_END" end-input)
      (org-delete-property "TIMELINE_END"))
    (save-buffer)
    (org-roam-db-sync)
    ;; Auto-show the node we just edited
    (org-roam-timeline-show-node)))

(defun org-roam-timeline--get-prop (key props)
  (let ((match (seq-find (lambda (pair) (string-equal-ignore-case (car pair) key)) props)))
    (cdr match)))

(defun org-roam-timeline--clean-date (date-str &optional is-end)
  (when (stringp date-str)
    (let ((clean (string-trim date-str)))
      (cond
       ((string-match "^-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" clean) clean)
       ((string-match "^\\(-?[0-9]\\{4\\}\\)$" clean)
        (if is-end (concat (match-string 1 clean) "-12-31") (concat (match-string 1 clean) "-01-01")))
       ((string-match "[\\[<]\\(-?[0-9]+\\S-*\\)[\\]>]" clean) (match-string 1 clean))
       (t nil)))))

;; --- 2. Data Logic ---
(defun org-roam-timeline--process-node (node)
  "Convert a single org-roam NODE into a timeline alist. Returns nil if invalid."
  (condition-case err
      (let* ((props (org-roam-node-properties node))
             (id (org-roam-node-id node))
             (start-raw (org-roam-timeline--get-prop "TIMELINE_START" props))
             (end-raw   (org-roam-timeline--get-prop "TIMELINE_END" props))
             (start-clean (org-roam-timeline--clean-date start-raw nil))
             (end-clean   (org-roam-timeline--clean-date end-raw t)))

        (when start-clean
          (let* ((tags (or (org-roam-node-tags node) '("Uncategorized")))
                 (backlinks (org-roam-backlinks-get node))
                 (incoming-ids (mapcar (lambda (bl) (org-roam-node-id (org-roam-backlink-source-node bl))) backlinks))
                 (outgoing-ids (ignore-errors
                                 (mapcar #'car (org-roam-db-query [:select dest :from links :where (= source $s1)] id))))
                 (all-neighbors (append incoming-ids outgoing-ids))
                 (backlink-count (length backlinks))
                 (importance-class (cond ((> backlink-count 10) "item-huge") ((> backlink-count 5) "item-large") (t "item-small"))))
            
            `((id . ,id)
              (content . ,(org-roam-node-title node))
              (start . ,start-clean)
              ,@(when end-clean `((end . ,end-clean)))
              (type . ,(if end-clean "range" "point"))
              (className . ,importance-class)
              (all_tags . ,tags)
              (neighbors . ,all-neighbors)))))
    (error (message "TIMELINE SKIP: '%s' failed: %s" (org-roam-node-title node) err) nil)))

(defun org-roam-timeline--get-nodes ()
  (let ((nodes '()))
    (message "TIMELINE: Starting export...")
    (dolist (node (org-roam-node-list))
      (let ((item (org-roam-timeline--process-node node)))
        (when item (push item nodes))))
    (message "TIMELINE: Exported %d nodes." (length nodes))
    nodes))

;; --- 3. Servlets ---

;; A. BULK DATA
(defservlet* data text/json ()
  (insert (json-encode (org-roam-timeline--get-nodes))))

;; B. SINGLE NODE DATA
(defservlet* node-data text/json (id)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (let ((item (org-roam-timeline--process-node node)))
          (if item (insert (json-encode item)) (insert "{}"))) 
      (insert "{}"))))

;; C. OPEN IN EMACS
(defservlet* open text/plain (id)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (progn
          (with-current-buffer (window-buffer (selected-window))
            (org-roam-node-open node))
          (insert "Opened"))
      (insert "Node not found"))))

;; D. CURRENT FOCUS (Smart Polling)
(defservlet* current-focus text/plain ()
  (if org-roam-timeline--explicit-focus-id
      ;; Scenario 1: User explicitly asked to show a node (via M-x)
      (progn
        (insert org-roam-timeline--explicit-focus-id)
        (setq org-roam-timeline--explicit-focus-id nil)) ;; Reset after sending
    
    ;; Scenario 2: Passive Follow Mode (Cursor position)
    (let* ((user-window (selected-window))
           (user-buffer (window-buffer user-window))
           (node-id nil))
      (with-current-buffer user-buffer
        (when (derived-mode-p 'org-mode)
          (let ((node (org-roam-node-at-point)))
            (when node (setq node-id (org-roam-node-id node))))))
      (insert (or node-id "")))))

;; E. REMOVE DATE
(defservlet* remove-date text/plain (id)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (let ((file (org-roam-node-file node))
              (point (org-roam-node-point node)))
          (with-current-buffer (find-file-noselect file)
            (goto-char point)
            (org-delete-property "TIMELINE_START")
            (org-delete-property "TIMELINE_END")
            (save-buffer))
          (insert "Removed"))
      (insert "Node not found"))))

;; F. CONTENT PREVIEW (Fixed let* Bug)
(defservlet* content text/html (id)
  (require 'ox-html)
  (let ((marker (org-id-find id 'marker))
        (final-output ""))
    (if (not marker)
        (setq final-output (format "<h3>Error: ID %s not found.</h3>" id))
      
      ;; FIX: used let* so 'node' is defined before 'backlinks' uses it
      (let* ((raw-content 
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (let ((beg (point))
                        (end (if (org-at-heading-p)
                                 (save-excursion (org-end-of-subtree) (point))
                               (point-max))))
                    (buffer-substring-no-properties beg end)))))
             (node (org-roam-node-from-id id))
             (backlinks (org-roam-backlinks-get node)))
        
        (condition-case err
            (let ((org-export-use-babel nil)
                  (org-confirm-babel-evaluate nil)
                  (org-html-with-latex 'mathjax)
                  (org-export-with-section-numbers nil)
                  (org-html-link-org-files-as-html nil)) 
              (let ((html (org-export-string-as raw-content 'html t '(:with-toc nil :with-section-numbers nil))))
                (let ((bl-html "<div class='backlinks-section' style='margin-top:40px; border-top:1px solid #eee; padding-top:20px;'>
                                <h4 style='color:#888; text-transform:uppercase; font-size:12px; letter-spacing:1px;'>Linked References</h4>
                                <ul style='list-style:none; padding:0;'>"))
                  (dolist (bl backlinks)
                    (let* ((source-node (org-roam-backlink-source-node bl))
                           (src-id (org-roam-node-id source-node))
                           (src-title (org-roam-node-title source-node)))
                      (setq bl-html (concat bl-html 
                                            (format "<li style='margin-bottom:8px;'><a href='id:%s'>%s</a></li>" 
                                                    src-id src-title)))))
                  (setq bl-html (concat bl-html "</ul></div>"))
                  (setq final-output (concat "<div class='org-content'>" html bl-html "</div>")))))
          (error (setq final-output (format "<h3 style='color:red'>Export Failed</h3><pre>%s</pre>" err))))))
    (insert final-output)))

;; --- 4. Commands ---

(defun org-roam-timeline-open ()
  (interactive)
  (setq httpd-root (expand-file-name "html" org-roam-timeline-root))
  (httpd-start)
  (browse-url (format "http://localhost:%d" httpd-port)))

;; NEW: SETS FLAG FOR BROWSER (Does not open new tab)
(defun org-roam-timeline-show-node ()
  "Force the browser timeline to show/focus the current node."
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (if node
        (progn
          (setq org-roam-timeline--explicit-focus-id (org-roam-node-id node))
          (message "Signal sent. Check browser."))
      (user-error "No node at point"))))

(provide 'org-roam-timeline)
