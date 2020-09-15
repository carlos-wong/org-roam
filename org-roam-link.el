;;; org-roam-link.el --- Custom links for Org-roam -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>
;;                  Alan Carroll

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This adds the custom `roam:' link to Org-roam. `roam:' links allow linking to
;; Org-roam files via their titles and headlines.
;;
;;; Code:
;;;; Dependencies

(require 'ol)

(defcustom org-roam-link-auto-replace t
  "When non-nil, replace Org-roam's roam links with file or id links whenever possible."
  :group 'org-roam
  :type 'boolean)

;; Install the custom link type
(org-link-set-parameters "roam"
                         :follow #'org-roam-link-follow-link)

(defun org-roam-link--split-path (path)
  "Splits PATH into title and headline.
Return a list of the form (type title has-headline-p headline star-idx).
type is one of `title', `headline', `title+headline'.
title is the title component of the path.
headline is the headline component of the path.
star-idx is the index of the asterisk, if any."
  (save-match-data
    (let* ((star-index (string-match-p "\\*" path))
           (title (substring-no-properties path 0 star-index))
           (headline (if star-index
                         (substring-no-properties path (+ 1 star-index))
                       ""))
           (type (cond ((not star-index)
                        'title)
                       ((= 0 star-index)
                        'headline)
                       (t 'title+headline))))
      (list type title headline star-index))))

(defun org-roam-link--get-location (link)
  "Return the location of Org-roam fuzzy LINK.
The location is returned as a list containing (link-type loc desc marker).
nil is returned if there is no matching location.

link-type is either \"file\" or \"id\".
loc is the target location: e.g. a file path, or an id.
marker is a marker to the headline, if applicable."
  (let (mkr link-type desc loc)
    (pcase-let ((`(,type ,title ,headline _) (org-roam-link--split-path link)))
      (pcase type
        ('title+headline
         (let ((file (org-roam--get-file-from-title title)))
           (if (not file)
               (org-roam-message "Cannot find matching file")
             (setq mkr (org-roam--get-id-from-headline headline file))
             (pcase mkr
               (`(,marker . ,target-id)
                (setq mkr marker
                      loc target-id
                      link-type "id"
                      desc headline))
               (_ (org-roam-message "cannot find matching id"))))))
        ('title
         (setq loc (org-roam--get-file-from-title title)
               desc title
               link-type "file")
         (when loc (setq loc (file-relative-name loc))))
        ('headline
         (setq mkr (org-roam--get-id-from-headline headline))
         (pcase mkr
           (`(,marker . ,target-id)
            (setq mkr marker
                  loc target-id
                  desc headline
                  link-type "id"))
           (_ (org-roam-message "Cannot find matching headline")))))
      (list link-type loc desc mkr))))

(defun org-roam-link--roam-to-file-link (link-type loc desc)
  (save-excursion
    (save-match-data
      (unless (org-in-regexp org-link-bracket-re 1)
        (user-error "No link at point"))
      (replace-match "")
      (insert (org-link-make-string (concat link-type ":" loc) desc)))))

(defun org-roam-link-follow-link (path)
  "Navigates to location specified by PATH."
  (pcase-let ((`(,link-type ,loc ,desc ,mkr) (org-roam-link--get-location path)))
    (when (and org-roam-link-auto-replace loc desc)
      (org-roam-link--roam-to-file-link link-type loc desc))
    (pcase link-type
          ("file"
           (if loc
               (org-roam--find-file loc)
             (org-roam-find-file desc nil nil t)))
          ("id"
           (org-goto-marker-or-bmk mkr)))))

(defun org-roam-link-replace-all ()
  "Replace all roam links in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (let ((context (org-element-context)))
          (pcase (org-element-lineage context '(link) t)
            (`nil nil)
            (link
             (when (string-equal "roam" (org-element-property :type link))
               (pcase-let ((`(,link-type ,loc ,desc _) (org-roam-link--get-location (org-element-property :path link))))
                 (when (and link-type loc)
                   (org-roam-link--roam-to-file-link link-type loc desc))))))))))

(defun org-roam-link--replace-link-on-save ()
  "Hook to replace all roam links on save."
  (when org-roam-link-auto-replace
    (org-roam-link-replace-all)))

(defun org-roam-link-complete-at-point ()
  "Do appropriate completion for the link at point."
  (let ((end (point))
        (start (point))
        (exit-fn (lambda (&rest _) nil))
        collection)
    (when (org-in-regexp org-link-bracket-re 1)
      (setq start (+ (match-beginning 1) (length "roam:"))
            end (match-end 1))
      (let ((context (org-element-context)))
        (pcase (org-element-lineage context '(link) t)
          (`nil nil)
          (link (when (string-equal "roam" (org-element-property :type link))
                  (pcase-let ((`(,type ,title _ ,star-idx)
                               (org-roam-link--split-path (org-element-property :path link))))
                    (pcase type
                      ('title+headline
                       (when-let ((file (org-roam--get-file-from-title title t)))
                         (setq collection (apply-partially #'org-roam--get-headlines file))
                         (setq start (+ start star-idx 1))))
                      ('title
                       (setq collection #'org-roam--get-titles))
                      ('headline
                       (setq collection #'org-roam--get-headlines)
                       (setq start (+ start star-idx 1))))))))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-dynamic
                   (lambda (_)
                     (cl-remove-if (apply-partially #'string= prefix)
                                   (funcall collection))))
                collection)
              :exit-function exit-fn)))))

(provide 'org-roam-link)
