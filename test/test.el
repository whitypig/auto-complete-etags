(require 'auto-complete-etags)
(require 'el-expectations)

(eval-when-compile
  (require 'cl))

;; Testing function for ac-etags-search-for-documentation
(defun test-ac-etags-search-for-documentation (mode item &optional tagfile)
  (let ((ret nil) (major-mode mode) (org-name tags-file-name) (org-list tags-table-list)
        (tagfile (and tagfile (expand-file-name tagfile))))
    (and tagfile (setq tags-table-list `(,tagfile)))
    (setq ret (ac-etags-search-for-documentation item))
    (setq tags-file-name org-name)
    (setq tags-table-list org-list)
    ret))

;; Tests for ac-etags-search-for-signature
(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func" "c.TAGS"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)"
    (test-ac-etags-search-for-documentation 'c-mode "simple_func2" "c.TAGS"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)"
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_func" "c.TAGS"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_va_arg_func" "c.TAGS"))

  (desc "Test for a function with macro")
  (expect "MACRO1 MACRO2 const char * macro_func(int a)"
    (test-ac-etags-search-for-documentation 'c-mode "macro_func" "c.TAGS"))

  (desc "No documentation found")
  (expect "No documentation found."
    (test-ac-etags-search-for-documentation 'c-mode "foo" "c.TAGS"))

  ;; For now, we ignore old-style functions.
  ;; (desc "Test for old-style function declaration")
  ;; (expect "void old_style_func(a, b) int a; int b;"
  ;;   (test-ac-etags-search-for-documentation 'c-mode "old_style_func" "c.TAGS"))
  )

;; Test when TAGS has changed.
(expectations
  (desc "Completing from c.TAGS")
  (expect "void simple_func(void)"
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "simple_func"))

  (desc "Completing from c.another.TAGS")
  (expect "static const char *g(void)"
    (visit-tags-table (expand-file-name "c.another.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "g"))

  ;; Switching again
  (desc "Completing from c.TAGS")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (test-ac-etags-search-for-documentation 'c-mode "multiple_line_va_arg_func"))
  )

;; Test for completion in the mode that is not the same as the source file.
(expectations
  (desc "Completing from .h file in emacs-lisp-mode.")
  ;; Currently, this expect fails.
  (expect nil
    (visit-tags-table (expand-file-name "c.TAGS") t)
    (ac-etags-search-for-documentation "simple_func"))
  )
