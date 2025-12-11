;;; org-roam-timeline.el --- Timeline with Content Preview & Navigation

(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name)))

(defun org-roam-timeline--get-nodes ()
  (let ((nodes '()))
    (dolist (node (org-roam-node-list))
      (let* ((props (org-roam-node-properties node))
             (id (org-roam-node-id node))
             (start-raw (cdr (assoc "TIMELINE_START" props)))
             (end-raw   (cdr (assoc "TIMELINE_END" props)))
             (start-clean (org-roam-timeline--clean-date start-raw nil))
             (end-clean   (org-roam-timeline--clean-date end-raw t))
             (tags (org-roam-node-tags node))
             (tags-list (if tags tags '("Uncategorized")))
             (backlinks (org-roam-backlinks-get node))
             (neighbor-ids (mapcar (lambda (bl) 
                                     (org-roam-node-id (org-roam-backlink-source-node bl))) 
                                   backlinks))
             (backlink-count (length backlinks))
             (importance-class 
              (cond ((> backlink-count 10) "item-huge")
                    ((> backlink-count 5)  "item-large")
                    (t "item-small"))))

        (when start-clean
          (push `((id . ,id)
                  (content . ,(org-roam-node-title node))
                  (start . ,start-clean)
                  ,@(when end-clean `((end . ,end-clean)))
                  (type . ,(if end-clean "range" "point"))
                  (className . ,importance-class)
                  (all_tags . ,tags-list)
                  (neighbors . ,neighbor-ids)) 
                nodes))))
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

(defservlet* data text/json ()
  (let ((data (org-roam-timeline--get-nodes)))
    (insert (json-encode data))))

(defservlet* open text/plain (id)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (progn
          (with-current-buffer (window-buffer (selected-window))
            (org-roam-node-open node))
          (insert "Opened"))
      (insert "Node not found"))))

(defservlet* content text/html (id)
  (require 'ox-html)
  (let ((node (org-roam-node-from-id id)))
    (if node
        (let ((file (org-roam-node-file node))
              (point (org-roam-node-point node)))
          (condition-case err
              (with-temp-buffer
                (insert-file-contents file)
                (org-mode)
                (goto-char point)
                (if (org-at-heading-p)
                    (org-narrow-to-subtree)
                  nil)
                (let ((org-html-with-latex 'mathjax)
                      (org-export-with-section-numbers nil)
                      (org-html-mathjax-options
                       '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")))
                      (html (org-export-as 'html nil nil 'body-only)))
                  (insert html)))
            (error (insert (format "<div class='error'>Error: %s</div>" err)))))
      (insert "<h3>Node not found</h3>"))))

(defun org-roam-timeline-open ()
  (interactive)
  (setq httpd-root (expand-file-name "html" org-roam-timeline-root))
  (httpd-start)
  (browse-url (format "http://localhost:%d" httpd-port)))

(provide 'org-roam-timeline)
