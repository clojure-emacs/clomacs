(ert-deftest clomacs-defun-test ()
  "Tests for `clomacs-defun'."
  (clomacs-defun summ-1 +)
  (should (equal (summ-1 2 3) "5"))

  (clomacs-defun summ-2 + :return-type :number)
  (should (equal (summ-2 2 3) 5))

  (clomacs-defun summ-3 + :return-type string-to-int)
  (should (equal (summ-3 2 3) 5))

  (clomacs-defun str str)
  (should (equal (str 2 "str") "2str"))

  (clomacs-defun get-property System/getProperty)
  (should (stringp (get-property "java.version")))

  (clomacs-defun make-clojure-list list :return-type :list)
  (should (equal '(1 2 3) (make-clojure-list 1 2 3)))

  (clomacs-defun make-clojure-vector vector :return-type :list)
  (should (equal '[1 2 3] (make-clojure-vector 1 2 3))))

(ert-deftest clomacs-integration-test ()
  "Integration test for `clomacs'."
  (require 'clomacs)

  (clomacs-defun clomacs-test-md-wrapper
                 my-md-to-html-string
                 :lib-name "clomacs"
                 :namespace clomacs.cm-test
                 :doc "Convert markdown to html via clojure lib.")

  (should (equal
           (clomacs-test-md-wrapper "# This is a test")
           "<h1>This is a test</h1>")))
