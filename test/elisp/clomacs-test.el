(require 'cl)

(cl-labels ((concat-path (&rest folders)
                         ;; Concatenate list of folders to the path.
                         (let ((path))
                           (dolist (folder folders)
                             (if folder
                                 (setq path (expand-file-name folder path))))
                           path)))
  (let ((current-directory (file-name-directory (or load-file-name ""))))
    (setq clomacs-test-path (expand-file-name "." current-directory))
    (setq clomacs-root-path (concat-path current-directory
                                         ".." ".." "src" "elisp"))))

(add-to-list 'load-path clomacs-root-path)
(add-to-list 'load-path clomacs-test-path)

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'clomacs)

(ert-deftest clomacs-defun-test ()
  "Tests for `clomacs-defun'."
  (clomacs-defun summ-1 +)
  (should (equal (summ-1 2 3) "5"))

  (clomacs-defun summ-async +
                 :call-type :async
                 :callback (lambda (result)
                             (should (numberp result))
                             (should (equal result (+ 3 5 9))))
                 :return-type :number)
  (summ-async 3 5 9)

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

(if noninteractive
    (ert-run-tests-batch-and-exit t))
