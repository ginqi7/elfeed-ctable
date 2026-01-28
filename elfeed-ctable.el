;;; elfeed-ctable.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'ctable)
(require 'elfeed)
(require 'transient)

;;; Custom Varibales
(defcustom elfeed-ctable-header
  `((elfeed-ctable--entry-date . ,(make-ctbl:cmodel :title "Date"  :align 'left))
    (elfeed-entry-title . ,(make-ctbl:cmodel :title "Title" :align 'left))
    (elfeed-ctable--entry-author . ,(make-ctbl:cmodel :title "Author" :align 'left))
    (elfeed-entry-tags . ,(make-ctbl:cmodel :title "Tags" :align 'left)))
  "Defines a customizable variable specifying the header columns for the elfeed ctable, associating column symbols with their display models including title and alignment."
  :type '(alist :key-type symbol
                :value-type (sexp :tag "ctbl:cmodel"))
  :group 'elfeed-ctable)
;;; Internal Varaibales

(setq elfeed-search--offset 2)

;;; Internal Functions

(defun elfeed-ctable--entry-author (entry)
  "Retrieves the author of an elfeed entry by returning the title of the feed associated with the entry."
  (elfeed-feed-title (elfeed-entry-feed entry)))

(defun elfeed-ctable--entry-date (entry)
  "Formats and returns the date of an elfeed entry for display by applying elfeed's date formatting function to the entry's date."
  (elfeed-search-format-date (elfeed-entry-date entry)))

(defun elfeed-ctable-click (cp)
  "Handles a click event on the ctable by retrieving the selected row and displaying the corresponding elfeed entry."
  (let* ((row (ctbl:cp-get-selected-data-row cp)))
    (elfeed-show-entry (car (last row)))))

(defun elfeed-ctable-insert-entries (entries)
  "Inserts entries into the ctable by formatting entry data according to the column model, creating a table component with click handling for entry interaction."
  (let ((data)
        (column-model (mapcar #'cdr elfeed-ctable-header))
        (mode)
        (cp))
    (dolist (entry entries)
      ;; (print entry)
      (push (append (mapcar (lambda (item) (funcall (car item) entry)) elfeed-ctable-header)
                    (list entry))
            data))
    (setq mode (make-ctbl:model :column-model column-model :data (reverse data)))
    (setq cp (ctbl:create-table-component-region :model mode))
    (ctbl:cp-add-click-hook
     cp (lambda () (elfeed-ctable-click cp)))))

(defun elfeed-search-update (&optional force)
  "Update the elfeed-search buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (when (or force (and (not elfeed-search-filter-active)
                         (< elfeed-search-last-update (elfeed-db-last-update))))
      (elfeed-save-excursion
        (let ((inhibit-read-only t)
              (standard-output (current-buffer)))
          (erase-buffer)
          (elfeed-search--update-list)
          (elfeed-ctable-insert-entries elfeed-search-entries)
          (setf elfeed-search-last-update (float-time))))
      (when (zerop (buffer-size))
        ;; If nothing changed, force a header line update
        (force-mode-line-update))
      (run-hooks 'elfeed-search-update-hook))))

(provide 'elfeed-ctable)
;;; elfeed-ctable.el ends here
