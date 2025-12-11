;;; org-roam-timeline.el --- Visualize Org-roam notes on a timeline -*- lexical-binding: t; -*-

;; Author: Gerardo gc597@cornell.edu
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (simple-httpd "1.5.1"))
;; Keywords: org-roam, visualization, timeline

;;; Commentary:
;; A tool to visualize org-roam nodes on an interactive timeline.

(require 'org-roam)
(require 'simple-httpd)
(require 'json)

;; -- Configuration --
(defgroup org-roam-timeline nil
  "Settings for org-roam-timeline."
  :group 'org-roam)

(defcustom org-roam-timeline-prop-start "TIMELINE_START"
  "The property name for the start date."
  :type 'string
  :group 'org-roam-timeline)

(defcustom org-roam-timeline-prop-end "TIMELINE_END"
  "The property name for the end date."
  :type 'string
  :group 'org-roam-timeline)

;; -- Interactive Functions --

(defun org-roam-timeline-add-date ()
  "Interactively add timeline properties using raw string input.
   Uses read-string instead of org-read-date to support pre-1970 and BC dates."
  (interactive)
  (let* ((start-input (read-string "Start Date (YYYY-MM-DD, YYYY, or -YYYY): "))
         (is-range (y-or-n-p "Is this a time range (period)? "))
         (end-input (if is-range
                        (read-string "End Date: ")
                      nil)))
    
    ;; Simple validation to ensure we don't save empty strings
    (unless (string-empty-p start-input)
      (org-set-property org-roam-timeline-prop-start start-input))
    
    (if (and end-input (not (string-empty-p end-input)))
        (org-set-property org-roam-timeline-prop-end end-input)
      (org-delete-property org-roam-timeline-prop-end))))

(provide 'org-roam-timeline)

;; -- Data Extraction Logic --

(defun org-roam-timeline--get-nodes ()
  "Extract nodes with 'all_tags' explicitly for JS filtering."
  (let ((nodes '()))
    (dolist (node (org-roam-node-list))
      (let* ((props (org-roam-node-properties node))
             (id (org-roam-node-id node))
             
             ;; Date Cleaning
             (start-raw (cdr (assoc "TIMELINE_START" props)))
             (end-raw   (cdr (assoc "TIMELINE_END" props)))
             (start-clean (org-roam-timeline--clean-date start-raw nil))
             (end-clean   (org-roam-timeline--clean-date end-raw t))
             
             ;; Tag Handling
             (tags (org-roam-node-tags node))
             ;; Force a list even if empty, ensuring JSON array
             (tags-list (if tags tags '("Uncategorized")))
             
             ;; Importance / Sizing
             (backlink-count (length (org-roam-backlinks-get node)))
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
                  ;; CRITICAL: This sends the list ["tag1", "tag2"] to JS
                  (all_tags . ,tags-list)) 
                nodes))))
    nodes))

;; Clean date strings to support BC years and YYYY-only precision

(defun org-roam-timeline--clean-date (date-str &optional is-end)
  "Clean dates. If only a year is given:
   - Start dates become YYYY-01-01
   - End dates become YYYY-12-31 (to include the full year)."
  (when (stringp date-str)
    (let ((clean (string-trim date-str)))
      (cond
       ;; Case 1: Standard YYYY-MM-DD (already precise, keep it)
       ((string-match "^-?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" clean)
        clean)
       
       ;; Case 2: Year only (e.g. 1735 or -0300)
       ((string-match "^\\(-?[0-9]\\{4\\}\\)$" clean)
        (if is-end
            (concat (match-string 1 clean) "-12-31") ;; End of year
          (concat (match-string 1 clean) "-01-01"))) ;; Start of year
       
       ;; Case 3: Org Timestamp (strip brackets)
       ((string-match "[\\[<]\\(-?[0-9]+\\S-*\\)[\\]>]" clean)
        (match-string 1 clean))
       
       (t nil)))))

;; -- JSON Export Function --

(defun org-roam-timeline-export-json ()
  "Export timeline nodes to a JSON string."
  (interactive)
  (let* ((data (org-roam-timeline--get-nodes))
         (json-str (json-encode data)))
    (message "Timeline Data Exported: %s" json-str)
    json-str))

;; -- Web Server Integration --

(defvar org-roam-timeline-root (file-name-directory (or load-file-name buffer-file-name))
  "The directory containing the package source code.")

(defun org-roam-timeline-open ()
  "Start the web server and open the timeline in the browser."
  (interactive)
  ;; 1. Configure simple-httpd to serve our 'html' folder
  (setq httpd-root (expand-file-name "html" org-roam-timeline-root))
  
  ;; 2. Start the server if not running (default port 8080)
  (httpd-start)
  
  ;; 3. Open the browser
  (browse-url (format "http://localhost:%d" httpd-port)))

;; -- API Endpoint --

(defservlet* data text/json ()
  "Serve the JSON data at http://localhost:8080/data"
  (let ((json-str (org-roam-timeline-export-json-string))) 
    (insert json-str)))

;; Helper to return string instead of printing message
(defun org-roam-timeline-export-json-string ()
  "Return the JSON string of timeline nodes."
  (let ((data (org-roam-timeline--get-nodes)))
    (json-encode data)))

;;; org-roam-timeline.el ends here
