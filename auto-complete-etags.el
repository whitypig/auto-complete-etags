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

(defadvice ac-inline-hide (around ac-etags-ac-inline-hide-around-ad activate)
  (when (and (member 'ac-source-etags 'ac-sources)
             (eq ac-buffer (current-buffer)))
    ad-do-it))

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

(defvar ac-etags-use-document t
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
  '((c-mode ac-etags-get-c-mode-document)))

(defconst ac-etags-document-not-found-message "No documentaion found.")

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

;; @todo What to do when multiple tags match item.
(defun ac-etags-search-for-signature (item)
  "Search for and return the signature for ITEM."
  (let* ((ret "No documentation found.") (case-fold-search nil)
        (b nil) (line nil) (mode 'c-mode) (buffers (buffer-list))
        ;; Shadow etags global variables because we don't want to change them.
        (tags-location-ring (make-ring find-tag-marker-ring-length))
        (find-tag-marker-ring (make-ring find-tag-marker-ring-length))
        (last-tag nil))
    (unwind-protect
        (when (and tags-table-list
                   (setq b (save-excursion (ignore-errors (find-tag-noselect item nil t)))))
          (setq ret (ac-etags-get-document-by-mode item b major-mode)))
      (when (and b (not (member b buffers)))
        (kill-buffer b)))
    ret))

(defun ac-etags-get-document-by-mode (item buffer mode)
  ;(message "DEBUG: ac-etags-get-document-for-mode, mode=%s, buffer=%s" mode buffer)
  (let ((f (cadadr (assoc mode ac-etags-document-functions))))
    (if f (funcall f item buffer)
      nil)))

(defun ac-etags-get-c-mode-document (item buffer)
  "Return document for item in BUFFER."
  (let ((doc ac-etags-document-not-found-message))
    (with-current-buffer buffer
      (setq line (thing-at-point 'line))
      (when (string-match "(" line)
        ;; This is probably a function.
        (when (string= item (buffer-substring-no-properties
                             (point)
                             (save-excursion
                               (skip-chars-forward "^(")
                               (point))))
          (forward-line -1))
        (setq doc (buffer-substring-no-properties (point)
                                                  (save-excursion (skip-chars-forward "^{;\\\\/")
                                                                  (point))))))
    (setq doc (replace-regexp-in-string ";" "" doc))
    (setq doc (replace-regexp-in-string "[ \n\t]+" " " doc))
    (setq doc (replace-regexp-in-string "\\(^ \\| $\\)" "" doc))
    doc))

(defun ac-etags-document (item)
  "Return documentation corresponding to ITEM."
  (when ac-etags-use-document
    (let ((sig (ac-etags-search-for-signature (substring-no-properties item))))
      (when (stringp sig)
        (message "%s"sig))
      sig)))

(defun ac-etags-collect-buffers-by-major-mode (mode)
  (let ((ret nil) (l (buffer-list)))
    (dolist (b l)
      (save-excursion
        (set-buffer b)
        (when (equal major-mode mode)
          (add-to-list 'ret b))))
    (nreverse ret)))

;; Define ac-source-etags
(ac-define-source etags
  '((candidates . ac-etags-candidate)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (document . ac-etags-document)
    (requires . 2)))

(provide 'auto-complete-etags)
;;; auto-complete-etags.el ends here
