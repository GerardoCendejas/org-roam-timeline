;;; org-roam-timeline.el --- Visual timeline for Org-Roam nodes  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2025 Gerardo Cendejas Mendoza

;; Author: Gerardo Cendejas Mendoza <gc597@cornell.edu>
;; Maintainer: Gerardo Cendejas Mendoza <gc597@cornell.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (json-mode "1.0") (simple-httpd "1.5.1"))
;; Keywords: org, hypermedia, visualization, timeline
;; URL: https://github.com/GerardoCendejas/org-roam-timeline

;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  This package provides a visual timeline interface for Org-Roam nodes.

;;; Code:


(require 'org-roam)
(require 'simple-httpd)
(require 'json)
(require 'ox-html)

(defconst org-roam-timeline-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory where the org-roam-timeline package is installed.")

;; --- SIGNALS ---
(defvar org-roam-timeline--explicit-focus-id nil
  "Store the ID of the node to be explicitly focused.")

(defvar org-roam-timeline--explicit-hide-id nil
  "Store the ID of the node to be explicitly hidden.")

(defvar org-roam-timeline--explicit-date-focus nil
  "Store the date string to zoom towards.")

(defvar org-roam-timeline--filter-signal nil
  "Signal to filter tags.  A cons cell of (action . tag).")

(defvar org-roam-timeline--toggle-follow-signal nil
  "Boolean signal to toggle Follow Mode.")

(defvar org-roam-timeline--toggle-preview-signal nil
  "Boolean signal to toggle the auto-preview panel.")

;; --- HELPER: Get All Tags ---
(defun org-roam-timeline--get-all-tags ()
  "Retrieve a list of all unique tags in the Org-roam database."
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
  "Process an Org-roam NODE to extract time and structure metadata."
  (condition-case _err
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
  "Retrieve and process all valid nodes for the timeline."
  (let ((nodes '()))
    (dolist (node (org-roam-node-list))
      (let ((item (org-roam-timeline--process-node node)))
        (when item (push item nodes))))
    nodes))

(defun org-roam-timeline--clean-date (date-str &optional is-end)
  "Clean and format DATE-STR.  If IS-END is non-nil, adjust to year end."
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
(defun org-roam-timeline-open ()
  "Start the web server and open the timeline in the browser."
  (interactive)
  (setq httpd-root (expand-file-name "html" org-roam-timeline-root))
  (httpd-start)
  (browse-url (format "http://localhost:%d" httpd-port)))

(defun org-roam-timeline-show-node ()
  "Focus the current node in the visual timeline."
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (if node
        (progn
          (setq org-roam-timeline--explicit-focus-id (org-roam-node-id node))
          (message "Timeline: Show Node"))
      (user-error "No node at point"))))

(defun org-roam-timeline-hide-node ()
  "Hide the current node from the visual timeline."
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (if node
        (progn
          (setq org-roam-timeline--explicit-hide-id (org-roam-node-id node))
          (message "Timeline: Hide Node"))
      (user-error "No node at point"))))

(defun org-roam-timeline-toggle-follow ()
  "Toggle Follow Mode in the visualization."
  (interactive)
  (setq org-roam-timeline--toggle-follow-signal t)
  (message "Timeline: Toggled Follow Mode"))

(defun org-roam-timeline-toggle-preview ()
  "Toggle Auto-Preview (sidebar content) in the browser."
  (interactive)
  (setq org-roam-timeline--toggle-preview-signal t)
  (message "Timeline: Toggled Auto-Preview"))

(defun org-roam-timeline-tag-add ()
  "Add a tag to the current node, with completion for existing tags."
  (interactive)
  (let* ( ;;(node (org-roam-node-at-point))  ;; Not needed since org-roam-tag-add works on current buffer, unused variable
         (existing-tags (org-roam-db-query [:select :distinct tag :from tags]))
         (flat-tags (mapcar #'car existing-tags))
         (choice (completing-read "Tag: " flat-tags nil nil nil nil)))
    (if (string-blank-p choice)
        (message "No tag.")
      (org-roam-tag-add (list choice))
      (message "Tag '%s' added." choice))))

(defun org-roam-timeline-add-date ()
  "Interactively add TIMELINE_START/END properties to the current node."
  (interactive)
  (let* ((start-input (read-string "Start (YYYY or YYYY-MM-DD): "))
         (is-range (y-or-n-p "Is this a time range? "))
         (end-input (if is-range (read-string "End (YYYY or YYYY-MM-DD): ") nil)))
    (unless (string-empty-p start-input)
      (org-set-property "TIMELINE_START" start-input))
    (if end-input
        (org-set-property "TIMELINE_END" end-input)
      (org-delete-property "TIMELINE_END"))
    (save-buffer)
    (org-roam-db-sync)
    (org-roam-timeline-show-node)))

(defun org-roam-timeline-zoom-date ()
  "Zoom the timeline to a specific date."
  (interactive)
  (let ((date-str (read-string "Zoom to Date (YYYY[-MM[-DD]]): ")))
    (unless (string-empty-p date-str)
      (setq org-roam-timeline--explicit-date-focus date-str)
      (message "Timeline: Zooming to %s..." date-str))))

(defun org-roam-timeline-filter-toggle ()
  "Toggle the visibility of a specific tag in the timeline."
  (interactive)
  (let ((tag (completing-read "Toggle Tag: " (org-roam-timeline--get-all-tags) nil t)))
    (setq org-roam-timeline--filter-signal (cons "filter-toggle" tag))
    (message "Timeline: Toggling %s" tag)))

(defun org-roam-timeline-filter-block ()
  "Block (force hide) a specific tag in the timeline."
  (interactive)
  (let ((tag (completing-read "Block Tag: " (org-roam-timeline--get-all-tags) nil t)))
    (setq org-roam-timeline--filter-signal (cons "filter-block" tag))
    (message "Timeline: Blocking %s" tag)))

(defun org-roam-timeline-filter-reset ()
  "Reset all visualization filters."
  (interactive)
  (setq org-roam-timeline--filter-signal (cons "filter-reset" "all"))
  (message "Timeline: Resetting filters"))

(defun org-roam-timeline-filter-hide-all ()
  "Hide all tags (blank slate)."
  (interactive)
  (setq org-roam-timeline--filter-signal (cons "filter-hide-all" "all"))
  (message "Timeline: Hiding all tags (Blank Slate)."))(provide 'org-roam-timeline)

;;; org-roam-timeline.el ends here
