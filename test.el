(require 'auto-complete-etags)
(require 'el-expectations)

(eval-when-compile
  (require 'cl))

;; ac-etags-get-tags-location
(expectations
  (desc "Single location")
  (expect `((,(expand-file-name "test.c") 1))
    (ac-etags-get-tags-location "simple_func" (expand-file-name "c.TAGS")))

  (desc "Multiple locatioins")
  (expect `((,(expand-file-name "test.cc") 8) (,(expand-file-name "test.cc") 9))
    (ac-etags-get-tags-location "overloaded_func" (expand-file-name "cc.TAGS")))

  (desc "No entry")
  (expect nil
    (ac-etags-get-tags-location "none" (expand-file-name "cc.TAGS"))))

;; ac-etags-search-for-signature
(defun test-ac-etags-search-for-documentation (mode item tagfile)
  (let ((ret nil) (major-mode mode) (org-name tags-file-name) (org-list tags-table-list)
        (tagfile (expand-file-name tagfile)))
    (setq tags-table-list `(,tagfile))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(expectations
  (desc "Normal function")
  (expect "void normal_func()"
    (test-ac-etags-search-for-documentation 'c++-mode "normal_func" "cc.TAGS"))

  (desc "Getter")
  (expect "int get() const"
    (test-ac-etags-search-for-documentation 'c++-mode "get" "cc.TAGS"))

  (desc "Setter")
  (expect "void set(int i)"
    (test-ac-etags-search-for-documentation 'c++-mode "set" "cc.TAGS"))

  (desc "Overloaded functions")
  (expect "void overloaded_func(int i)\nvoid overloaded_func(double d)"
    (test-ac-etags-search-for-documentation 'c++-mode "overloaded_func" "cc.TAGS")))
