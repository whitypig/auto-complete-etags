(require 'auto-complete-etags)
(require 'el-expectations)

;; ac-etags-search-for-signature
(defun test-ac-etags-search-for-signature (item)
  (let ((ret nil) (major-mode 'c-mode) (org-name tags-file-name) (org-list tags-table-list))
    (setq tags-file-name "./TAGS")
    (setq ret (ac-etags-search-for-signature item))
    (setq tags-file-name org-name)
    (setq tags-table-list org-list)
    ret))

;; Synopsis: (expect expected actual)

;; Tests for ac-etags-search-for-signature
(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)" (test-ac-etags-search-for-signature "simple_func"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)" (test-ac-etags-search-for-signature "simple_func2"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)" (test-ac-etags-search-for-signature "multiple_line_func"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (test-ac-etags-search-for-signature "multiple_line_va_arg_func"))

  (desc "No documentation found")
  (expect "No documentation found."
    (test-ac-etags-search-for-signature "foo"))
  )

;; Tests for ac-etags-is-function
(expectations
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f(void)"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f()"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f() {"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f(int a, const char *p, ...)"))

  (desc "Maybe a function")
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f(int a,"))
  (expect nil (ac-etags-is-function-maybe "mcr" "#define mcr(e) #(e)"))
  (expect nil (ac-etags-is-function-maybe "g" "int g(void)\\")))

;; Test for ac-etags-get-c-mode-document
(defun test-ac-etags-get-c-mode-document (item)
  (let ((b nil) (org-name tags-file-name) (ret nil)
        (tags-file-name "./TAGS"))
    (setq b (find-tag-noselect item nil t))
    (setq ret (ac-etags-get-c-mode-document item b))
    (setq tags-file-name org-name)
    ret))

(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)" (test-ac-etags-get-c-mode-document "simple_func"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)" (test-ac-etags-get-c-mode-document "simple_func2"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)" (test-ac-etags-get-c-mode-document "multiple_line_func"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (test-ac-etags-get-c-mode-document "multiple_line_va_arg_func"))

  (desc "No documentation found")
  (expect (error)
    (test-ac-etags-get-c-mode-document "foo"))
  )
