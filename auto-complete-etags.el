;;; auto-complete-etags.el --- 

;; Copyright 2009 Yen-Chin,Lee
;;           2010, 2011 whitypig <whitypig@gmail.com>
;;

;; Author: Yen-Chin,Lee
;;         whtiypig <whitypig@gmail.com>
;; Keywords: auto-complete-mode etags
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; TODO: Put searched signatures into cache.
;; BUG: Source file which has searching tag is somehow modified.
;; BUG: After showing document, extra new lines are inserted in the current buffer.
;; BUG: Unnecessary marks are set.
;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-etags)

;;; Code:

(require 'auto-complete)
(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ac-etags-candidates-limit 40
  "The number of candidates to popup.
nil means there is no limit about it.")

(defvar ac-etags-use-document nil
  "Set this to t when you want to see sunction signatures.")

(defvar ac-etags-tags-current-completion-table nil
  "Current etags completion table for tags.")

(defvar ac-etags-current-tags-file-name tags-file-name
  "The name of the currently-chosen tags file name.

`tags-file-name' is defined in etasg.el")

(defvar ac-etags-current-tags-table-list tags-table-list
  "The name of the currently-chosen tags table.

`tags-table-list is defined in etags.el'")

(defvar ac-etags-document-functions
  '((c-mode . ac-etags-get-c-mode-document)))

(defconst ac-etags-document-not-found-message "No documentation found.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @bug Even if the tag file has been updated,
;; ac-etags-tags-current-completion-table won't be updated.
(defun ac-etags-init ()
  "Initialization function for ac-etags."
  (unless (and
           ;; tags-file-name and tags-table-list are defined in `etags.el'
           (equal tags-table-list
                  ac-etags-current-tags-table-list)
           (equal tags-file-name
                  ac-etags-current-tags-file-name))
    ;; When tags-file-name or list has changed, we create a new completion table.
    (let ((tags-completion-table nil))
      (setq ac-etags-tags-current-completion-table (tags-completion-table))
      (setq ac-etags-current-tags-file-name tags-file-name)
      (setq ac-etags-current-tags-table-list tags-table-list))))

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

;; @param item The name to be searched for in tagfile.
;; @param tag-file The absolute pathname of tag-file to be visited.
(defun ac-etags-get-tags-location (item tag-file)
  "Return a list consisting of information with which we try to find
definitions of ITEM. Its car is an abosolute pathname and its cadr is
line-number."
  (let ((b (find-file-noselect tag-file))
        (loc nil) (filename nil) (linenum nil))
    (unless b
      (error "ac-etags: Cannot find file: %s" tag-file))
    (unless (and (stringp tag-file) (file-name-absolute-p tag-file))
      (erro "ac-etags: The name of tag file is not absolute"))
    (save-excursion
      (set-buffer b)
      (goto-char (point-min))
      (when (re-search-forward (concat "" item "\\([0-9]+\\),[0-9]+$") nil t)
        (setq linenum (string-to-number (match-string 1)))
        ;; Search for the filename containing this item
        (if (re-search-backward "^\\([^[:cntrl:]]+\\),[0-9]+$" nil t)
            (setq filename (match-string 1))
          (error "ac-etags: Cannot find the source file for tag \"%s\"" item))
        (unless (file-name-absolute-p filename)
          (setq filename (replace-regexp-in-string "/[^/]+$" (concat "/" filename) tag-file t)))))
    (if (and filename linenum)
        (list filename linenum)
      nil)))

;; @todo What to do when multiple tags match item.
(defun ac-etags-search-for-documentation (item)
  "Search for and return the documentation about ITEM."
  (let* ((ret ac-etags-document-not-found-message) (case-fold-search nil)
         (loc nil) (mode major-mode))
    (when tags-table-list
      (block found
        (dolist (tagfile tags-table-list)
          (setq loc (ac-etags-get-tags-location item tagfile))
          ;; Check to see if this file is to be opened with the same major mode as MODE.
          (when (and loc (eq mode (assoc-default (car loc) auto-mode-alist 'string-match)))
            (return-from found)))))
    ;; loc => (filename line-number)
    ;; We try to find doc only when filename is an absolute pathname.
    (when (and loc (stringp (car loc)) (file-name-absolute-p (car loc)))
      (setq ret (ac-etags-get-document-by-mode item loc mode)))
    ret))

(defun ac-etags-get-document-by-mode (item location mode)
  (let ((f (cdr (assoc mode ac-etags-document-functions))))
    (if f (funcall f item (car location) (cadr location))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For c-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ac-etags-get-c-mode-document (item filename linenum)
  "Return documentation about ITEM defined in file FILENAME on
line number LINENUM."
  (let ((doc ac-etags-document-not-found-message) (beg nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (forward-line (1- linenum))
      (setq line (thing-at-point 'line))
      (unless (string-match item line)
        (error "ac-etags: Cannot find %s" item))
      ;; We are concerned with only fucntion-like structures.
      (when (string-match (concat item "(") line)
        (when (string-match (concat "^" item) line)
          (or (re-search-backward "\\([};/]\\|^$\\)" nil t) (goto-char (point-min))))
        (beginning-of-line)
        (setq beg (point))
        (skip-chars-forward "^{;\\\\/")
        (setq doc (buffer-substring-no-properties beg (point)))
        (setq doc (replace-regexp-in-string ";" "" doc))
        (setq doc (replace-regexp-in-string "[ \n\t]+" " " doc))
        (setq doc (replace-regexp-in-string "\\(^ \\| $\\)" "" doc))))
    doc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-mode ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac-etags-document (item)
  "Return documentation corresponding to ITEM. If no
documentation is found, return nil."
  (when ac-etags-use-document
    (let ((sig (ac-etags-search-for-documentation (substring-no-properties item))))
      (when (stringp sig)
        (message "%s"sig))
      sig)))

;; Define ac-source-etags
(ac-define-source etags
  '((candidates . ac-etags-candidate)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (document . ac-etags-document)
    (requires . 2)))

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here