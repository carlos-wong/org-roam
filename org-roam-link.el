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

(defun org-roam-link-follow-link (path)
  "Navigates to location specified by PATH."
  (pcase-let ((`(,type ,title ,headline _) (org-roam-link--split-path path)))
    (pcase type
      ('title+headline
       (if-let ((file (org-roam--get-file-from-title title)))
           (if-let ((mkr (org-roam--get-id-from-headline headline file)))
               (progn
                 (org-goto-marker-or-bmk (car mkr))
                 (setq (car mkr) nil))
             (org-roam-message "Cannot find matching id"))
         (org-roam-message "Cannot find matching file")))
      ('title
       (if-let ((file (org-roam--get-file-from-title title)))
           (org-roam--find-file file)
         (org-roam-find-file title nil nil t)))
      ('headline
       (if-let ((mkr (org-roam--get-id-from-headline headline)))
           (progn
             (org-goto-marker-or-bmk (car mkr))
             (setq (car mkr) nil))
         (org-roam-message "Cannot find matching headline"))))))
