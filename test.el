(require 'auto-complete-etags)
(require 'el-expectations)

;; ac-etags-search-for-signature
(defun test-ac-etags-search-for-documentation (item)
  (let ((ret nil) (major-mode 'c-mode) (org-name tags-file-name) (org-list tags-table-list)
        (tagfile (expand-file-name "./c.TAGS")))
    (setq tags-table-list `(,tagfile))
    (setq ret (ac-etags-search-for-documentation item))
    (setq tags-file-name org-name)
    (setq tags-table-list org-list)
    ret))

;; Synopsis: (expect expected actual)

;; Tests for ac-etags-search-for-signature
(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)" (test-ac-etags-search-for-documentation "simple_func"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)" (test-ac-etags-search-for-documentation "simple_func2"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)" (test-ac-etags-search-for-documentation "multiple_line_func"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (test-ac-etags-search-for-documentation "multiple_line_va_arg_func"))

  (desc "Test for a function with macro")
  (expect "MACRO1 MACRO2 const char * macro_func(int a)"
    (test-ac-etags-search-for-documentation "macro_func"))

  (desc "No documentation found")
  (expect "No documentation found."
    (test-ac-etags-search-for-documentation "foo"))

  ;; For now, we ignore old-style functions.
  ;; (desc "Test for old-style function declaration")
  ;; (expect "void old_style_func(a, b) int a; int b;"
  ;;   (test-ac-etags-search-for-documentation "old_style_func"))
  )
