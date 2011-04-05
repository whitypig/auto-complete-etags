(require 'auto-complete-etags)
(require 'el-expectations)

;; ac-etags-search-for-signature
(defun test-ac-etags-get-signature (item)
  (let ((signature nil)
        ;; shadow the original tags-table-list
        (tags-table-list '("e:/cygwin/home/whitypig/repos/git_repos/auto-complete-etags/TAGS")))
    (find-file-noselect (car tags-table-list))
    (save-excursion
      (setq signature (ac-etags-search-for-signature item)))
    signature))

(test-ac-etags-get-signature "simple_func")

;; Synopsis: (expect expected actual)

;; Tests for ac-etags-search-for-signature
(expectations
  (desc "Test for signature of a func-decl on single line")
  (expect "void simple_func(void)" (ac-etags-search-for-signature "simple_func"))

  (desc "Test for signature of a func-decl with multiple args")
  (expect "void simple_func2(int a, int b)" (ac-etags-search-for-signature "simple_func2"))

  (desc "Test for signature on multiple lines")
  (expect "int multiple_line_func(void)" (ac-etags-search-for-signature "multiple_line_func"))

  (desc "Test for signature of multiple args on multiple lines")
  (expect "const char* multiple_line_va_arg_func(int a, int b, ...)"
    (ac-etags-search-for-signature "multiple_line_va_arg_func"))

  (desc "No documentation found")
  (expect "No documentation found."
    (ac-etags-search-for-signature "foo")))

;; Tests for ac-etags-get-line
(expectations
  (expect "(require 'auto-complete-etags)"
    (save-excursion (goto-char (point-min))
                    (ac-etags-get-line (point)))))

;; Tests for ac-etags-get-return-type
(expectations
  (expect "(require 'auto-complete-etags)"
    (save-excursion (goto-char (point-min))
                    (forward-line)
                    (ac-etags-get-return-type))))

;; Tests for ac-etags-get-function-arguments
(expectations
  (expect " int b, ...)"
    (save-excursion
      (set-buffer (get-buffer "test.c"))
      (goto-char 151)
      (ac-etags-get-function-arguments))))

;; Tests for ac-etags-is-function
(expectations
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f(void)"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f()"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f() {"))
  (expect (non-nil) (ac-etags-is-function-maybe "f" "f(int a, const char *p, ...)"))
  (expect nil (ac-etags-is-function "f" "f(int a,"))
  (expect nil (ac-etags-is-function-maybe "mcr" "#define mcr(e) #(e)"))
  (expect nil (ac-etags-is-function-maybe "g" "int g(void)\\")))
