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
  (let ((ret "No documentation found.") (s nil) (case-fold-search nil))
    ;(messge (format "ac-etags-search-for-signature called with %s" item))
    (block ac-etags-search-for-signature-block
      (when (listp tags-table-list)
        (dolist (e tags-table-list)
          (save-excursion
            (set-buffer (get-file-buffer e))
            (goto-char (point-min))
            ;; @todo What to do when there are multiple signatures for ITEM?
            (when (re-search-forward (concat "^\\(.*(.*)\\)[;]*" item ".*$") nil t)
              ;; For now, we only search for a function having ITEM as its name.
              (setq s (buffer-substring (match-beginning 1) (match-end 1)))
              (setq ret (ac-etags-cleanup-document s))))
          (return-from ac-etags-search-for-signature-block ret))))
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