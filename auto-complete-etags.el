;;; auto-complete-etags.el --- 

;; Copyright 2009 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee
;; Version: $Id$
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
  "The name of the currently-chosen tags file name.")

(defvar ac-etags-current-tags-table-list tags-table-list
  "The name of the currently-chosen tags table.")

(defun ac-etags-init ()
  "Initialization function for ac-etags."
  (unless (or (equal tags-table-list
                     ac-etags-current-tags-table-list)
              (equal tags-file-name
                     ac-etags-current-tags-file-name))
    (setq ac-etags-tags-current-completion-table (tags-completion-table))
    (setq ac-etags-current-tags-file-name tags-file-name)
    (setq ac-etags-current-tags-table-list tags-table-list)))

(defun ac-etags-candidate ()
  (when (or tags-file-name tags-table-list)
    (ac-etags-init)
    ;; the following ignore-errors is commented out for debugging purpose
    ;(ignore-errors
      (let ((candidates (all-completions ac-target ac-etags-tags-current-completion-table))
            (ret nil))
        (cond
         ((and (numberp ac-etags-candidates-limit)
               (< ac-etags-candidates-limit (length candidates)))
          (dotimes (i ac-etags-candidates-limit)
            (add-to-list 'ret (nth i candidates) t)))
         (t
          (setq ret candidates)))
        ret)));)

(defvar ac-source-etags
  '((candidates . ac-etags-candidate)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 2))
  "Source for etags.")

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here
