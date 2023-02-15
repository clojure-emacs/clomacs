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
  (setq undercover-force-coverage t)
  (undercover "src/elisp/*.el"
              (:report-format 'lcov)
              (:merge-report nil)
              (:send-report nil)))

(require 'clomacs)

(setq cider-boot-parameters "repl -s -H localhost wait")
(setq cider-lein-parameters "repl :headless :host localhost")

(defun clomacs-get-emacs-major-version ()
  emacs-major-version)

(ert-deftest clomacs-string-to-boolean-test ()
  (should (equal (clomacs-string-to-boolean nil) nil))
  (should (equal (clomacs-string-to-boolean "nil") nil))
  (should (equal (clomacs-string-to-boolean "false") nil))
  (should (equal (clomacs-string-to-boolean "") t))
  (should (equal (clomacs-string-to-boolean "true") t))
  (should (equal (clomacs-string-to-boolean "any text") t)))

(ert-deftest clomacs-defun-test ()
  "Tests for elisp->clojure `clomacs-defun'."
  (clomacs-defun summ-1 +)
  (should (equal (summ-1 2 3) "5"))

  (clomacs-defun summ-async +
                 :call-type :async
                 :callback (lambda (result)
                             (should (numberp result))
                             (should (equal result (+ 3 5 9))))
                 :return-type :number)
  ;; (summ-async 3 5 9)

  (clomacs-defun summ-2 + :return-type :number)
  (should (equal (summ-2 2 3) 5))

  (clomacs-defun summ-3 + :return-type string-to-number)
  (should (equal (summ-3 2 3) 5))

  (clomacs-defun str str)
  (should (equal (str 2 "str") "2str"))

  (clomacs-defun get-property System/getProperty)
  (should (stringp (get-property "java.version")))

  (clomacs-defun make-clojure-list list :return-type :list)
  (should (equal '(1 2 3) (make-clojure-list 1 2 3)))

  (clomacs-defun make-clojure-vector vector :return-type :list)
  (should (equal '[1 2 3] (make-clojure-vector 1 2 3))))

(ert-deftest clomacs-defn-test ()
  "Tests for clojure->elisp `clomacs-defn'."
  (clomacs-defun clomacs-require clojure.core/require)
  (clomacs-require '[clojure.test :refer [run-tests]])
  (clomacs-require `'clomacs.core-test)
  (clomacs-defun run-tests run-tests :return-value :both)
  (clomacs-defun format-string clomacs/format-string)
  (clomacs-httpd-start)
  (let* ((lein-test-result (run-tests `'clomacs.core-test))
         (test-out (car lein-test-result))
         (test-value (eval (car (read-from-string
                                 (format-string (cdr lein-test-result)))))))
    (message test-out)
    (message "%s" test-value)
    (should (> (alist-get :test test-value) 0))
    (should (> (alist-get :pass test-value) 0))
    (should (= (alist-get :fail test-value) 0))
    (should (= (alist-get :error test-value) 0)))
  (clomacs-httpd-stop))

(ert-deftest clomacs-get-doc-test ()
  (should
   (or (equal "Wrapped clojure entity: clomacs/format-string
Format string created by Clojure side to Elisp structure as string."
              (clomacs-get-doc nil 'clomacs/format-string))
       (equal "Wrapped clojure entity: clomacs/format-string"
              (clomacs-get-doc nil 'clomacs/format-string)))))

(ert-deftest clomacs-integration-test ()
  "Integration test for `clomacs'."

  (clomacs-defun clomacs-test-text-with-newlines
                 text-with-newlines
                 :namespace clomacs.cm-test)

  (should (equal
           (clomacs-test-text-with-newlines)
           (concat "Some text in the first line \n"
                   "and text in the new line. "
                   "Text with backslash and \\no new line.")))

  (clomacs-defun clomacs-test-md-wrapper
                 my-md-to-html-string
                 :namespace clomacs.cm-test
                 :doc "Convert markdown to html via clojure lib.")

  (should (equal
           (clomacs-test-md-wrapper "# This is a test")
           "<h1>This is a test</h1>"))

  (should (equal
           (clomacs-test-md-wrapper "# This is \"a\" test")
           "<h1>This is \"a\" test</h1>")))

(ert-deftest clomacs-prepare-vars-test ()
  "Tests for `clomacs-prepare-vars'.
Expected: '(doc namespace-str cl-entity-full-name)"
  (should
   (equal
    '("doc" "cm-test.core" "cm-test.core/my-md-to-html-string")
    ;; Both :namespace is set and fully qualified function name
    (clomacs-prepare-vars 'cm-test.core/my-md-to-html-string
                          :doc "doc"
                          :namespace 'cm-test.core)))
  (should
   (equal
    '("doc" "cm-test.core" "cm-test.core/my-md-to-html-string")
    ;; Only fully qualified function name
    (clomacs-prepare-vars 'cm-test.core/my-md-to-html-string
                          :doc "doc")))
  (should
   (equal
    '("doc" "cm-test.core" "cm-test.core/my-md-to-html-string")
    ;; Only :namespace is set.
    (clomacs-prepare-vars 'my-md-to-html-string
                          :doc "doc"
                          :namespace 'cm-test.core))))

(clomacs-with-nrepl
 "clomacs"
 (lambda ()
   (if noninteractive
       (ert-run-tests-batch-and-exit t))))

(sit-for 60)
