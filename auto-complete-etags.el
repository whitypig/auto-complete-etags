;;; auto-complete-etags.el --- 

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Modified by: whitypig
;; Version: $Id: auto-complete-etags.el,v 1.8 2010/10/31 05:05:03 whitypig Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags)

;;; Code:

(require 'auto-complete)
(eval-when-compile
  (require 'cl))

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defvar ac-etags-candidates-limit 40
  "The number of candidates to popup.
nil means there is no limit about it.")

(defvar ac-etags-tags-current-completion-table nil
  "Current etags completion table for tags.")

(defvar ac-etags-current-tags-file-name tags-file-name
  "The name of the currently-chosen tags file name.

`tags-file-name' is defined in etasg.el")

(defvar ac-etags-current-tags-table-list tags-table-list
  "The name of the currently-chosen tags table.

`tags-table-list is defined in etags.el'")

(defun ac-etags-init ()
  "Initialization function for ac-etags."
  (unless (and
           ;; tags-file-name and tags-table-list are defined in `etags.el'
           (equal tags-table-list
                  ac-etags-current-tags-table-list)
           (equal tags-file-name
                  ac-etags-current-tags-file-name))
    (setq ac-etags-tags-current-completion-table (tags-completion-table))
    (setq ac-etags-current-tags-file-name tags-file-name)
    (setq ac-etags-current-tags-table-list tags-table-list)))

(defun ac-etags-candidate ()
  ;; These two variables are defined in `etags.el'
  (when (or tags-file-name tags-table-list)
    ;; If at least one tags table is selected, initialize completion table.
    (ac-etags-init)
      (let* ((candidates (all-completions ac-target ac-etags-tags-current-completion-table))
            (len (length candidates)))
        (when (and (numberp ac-etags-candidates-limit)
                    (< ac-etags-candidates-limit len))
          (nbutlast candidates (- len ac-etags-candidates-limit)))
        candidates)))

;; @bug When a function signature spans multiples lines, we cannot
;; find the signature.
(defun ac-etags-search-for-signature (item)
  "Search for and return the signature for ITEM."
  (let ((ret "No documentation found.") (case-fold-search nil)
        (b nil) (line nil))
    ;; For now, we only support c-mode.
    (when (and (equal major-mode 'c-mode)
               tags-table-list
               (setq b (save-excursion (ignore-errors (find-tag-noselect item nil t)))))
      ;; @todo We want to close the buffer if it was yet to be visited.
      (save-excursion
        (set-buffer b)
        (setq line (ac-etags-get-line (point)))
        (when (and line (ac-etags-is-function-maybe item line))
          (setq ret line)
          ;; Check if this line contains the return-type.
          (when (and ret (string-match (concat "^" item) ret))
            (setq ret (concat (ac-etags-get-return-type) " " ret)))
          ;; Check if this line contains all the arguments of this function.
          (when (and ret (not (string-match "[);]$" ret)))
            (setq ret (concat ret (ac-etags-get-function-arguments))))))
      (setq ret (replace-regexp-in-string ";" "" ret))
      (setq ret (replace-regexp-in-string "[ \n\t]+" " " ret)))
    ret))

(defun ac-etags-is-function-maybe (name line)
  "Return t if LINE contains \"NAME(.*)\"."
  (save-match-data
    (and (not (string-match "[#=/\\]" line))  ; exclude macro, enum, etc.
         (string-match (concat name "(") line))))

(defun ac-etags-get-line (point)
  "Return the line containing POINT."
  (let ((line nil))
    (buffer-substring-no-properties
     (save-excursion (beginning-of-line) (point))
     (save-excursion (end-of-line) (point)))))

(defun ac-etags-get-return-type ()
  "Return the line containing return-type.
We assume that current point must be on the function name. In
fact, this fucntion just returns a line one-line above `point'."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (ac-etags-get-line (point))))

(defun ac-etags-get-function-arguments ()
  "Return the string containing arguments declaration that
follows the current line."
  (let ((ret nil))
    (save-excursion
      (forward-line 1)
      (beginning-of-line)
      (setq ret (buffer-substring-no-properties (point) (re-search-forward ")"))))
    (setq ret (replace-regexp-in-string "[ \t\n]+" " " ret))
    ret))

(defun ac-etags-cleanup-document (str)
  "Replace multiples spaces with a single space."
  (when (stringp str)
    (setq str (replace-regexp-in-string "[ ]+" " " str))))

(defun ac-etags-document (item)
  "Return documentation corresponding to ITEM."
  (ac-etags-search-for-signature item))

;; Define ac-source-etags
(ac-define-source etags
  '((candidates . ac-etags-candidate)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (document . ac-etags-document)
    (requires . 2)))

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here