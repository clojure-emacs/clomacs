;;; clomacs.el --- Simplifies Emacs Lisp interaction with Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.3") (cider "0.16.0") (s "1.12.0") (simple-httpd "1.4.6"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `clomacs-defun' - core Clojure to Elisp function wrapper.
;;
;; See README.md for detailed description.


(require 'cl-lib)
(require 'cider)
(require 's)
(require 'simple-httpd)

(defvar clomacs-verify-nrepl-on-call t)
(defvar clomacs-autoload-nrepl-on-call t)
(defvar clomacs-httpd-port 8082
  "Http port to listen for requests from Clojure side.")

(defcustom clomacs-print-length 100000
  "Value for *print-length* set during `clomacs-defun' macros evaluation.
Restricts list length passed from Clojure to Emacs lisp.
Set `nil' for unlimited list length."
  :group 'clomacs
  :type 'integer)

(defcustom clomacs-restore-print-length nil
  "When t restore *print-length* acording to `cider-repl-print-length' value.
After any `clomacs-defun' wraped funtion call, restore *print-length*.
Can be useful for debugging purpose to run `clomacs-defun' functions and
Clojure code directly in the same REPL."
  :group 'clomacs
  :type 'boolean)

(defun cloamcs-get-dir (repl-info)
  (if repl-info
      (file-name-nondirectory
       (car (split-string repl-info ":")))))

(defun clomacs-search-connection (project-name)
  "Search nREPL connection buffer.
E.g. if you want to find \"*cider-repl clomacs-20160419.258*\" you shold pass
REPL-BUFFER-PROJECT-NAME \"clomacs\"."
  (let ((result nil))
    (maphash
     (lambda (k v)
       (let ((current-project-dir (cloamcs-get-dir (cdr k))))
         (if (and current-project-dir
                  (s-contains? project-name current-project-dir))
             (setq result (cadr v)))))
     sesman-sessions-hashmap)
    result))

(defun clomacs-get-connection (&optional library)
  "Return buffer with nREPL process related to LIBRARY.
If LIBRARY is nil, attempts to use \"clomacs\", \"localhost\" or
any current connection.
If can't find any nREPL process return nil."
  (if library
      (clomacs-search-connection library)
    (or (clomacs-search-connection "clomacs")
        (clomacs-search-connection "localhost")
        (cider-current-repl))))

(defun clomacs-get-session (connection)
  "Return current session for this CONNECTION."
  (assert connection)
  (with-current-buffer
    (set-buffer connection)
    (cider-current-session)))

(defun clomacs-launch-nrepl (library &optional sync)
  (let* ((starting-msg (format
                        "Starting nREPL server for %s..."
                        (propertize (or library "current-buffer")
                                    'face 'font-lock-keyword-face)))
         (lib-file (if library (find-library-name library)))
         (is-opened (if lib-file (find-buffer-visiting lib-file)))
         (lib-buff (or is-opened
                       (if lib-file
                           (find-file-noselect lib-file)))))
    ;; simple run lein
    (if lib-buff
        (with-current-buffer lib-buff
          (cider-jack-in nil))
      (cider-jack-in nil))
    (message starting-msg)
    (if sync
        (let ((old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
          (setq cider-repl-pop-to-buffer-on-connect nil)
          (while (not (clomacs-get-connection library))
            (sleep-for 0.1))
          (setq cider-repl-pop-to-buffer-on-connect old-cider-repl-pop)
          (if (and library (not is-opened))
              (kill-buffer lib-buff))))
    (message "Started.")))

(defun clomacs-return-stringp (raw-string)
  (and
   raw-string
   (equal (substring raw-string 0 1) "\"")
   (equal (substring raw-string
                     (1- (length raw-string)) (length raw-string)) "\"")))

(defun clomacs-strip-string (raw-string)
  (if (clomacs-return-stringp raw-string)
      (substring raw-string 1 (1- (length raw-string)))
    raw-string))

(defun clomacs-clean-result-string (return-string)
  (if (stringp return-string)
      (condition-case nil
          (s-replace-all '(("\\\\" . "\\")
                           ("\\\"" . "\"")
                           ("\\n"  . "\n")
                           ("\\t"  . "\t"))
                         return-string)
        (error return-string))
    return-string))

(defun clomacs-format-result (raw-string return-type)
  "Format Elisp representation of Clojure evaluation result."
  (cl-assert return-type)
  (if raw-string
      (let ((return-string (clomacs-strip-string raw-string)))
        (cond
         ((functionp return-type) (funcall return-type raw-string))
         ((eq return-type :string) (clomacs-clean-result-string return-string))
         ((eq return-type :int) (string-to-number return-string))
         ((eq return-type :number) (string-to-number return-string))
         ((eq return-type :list) (read raw-string))
         ((eq return-type :char) (string-to-char return-string))
         ((eq return-type :vector) (string-to-vector return-string))))))

(declare clomacs-format-arg)

(defun clomacs-plist-p (object)
  "Return t if OBJECT is a plist, otherwise, return nil."
  (when (and (listp object)
             (car object)
             (listp (car object))
             (not (listp (cdr (car object)))))
    t))

(defun clomacs-plist-to-map (lst)
  "Build string representation of Clojure map from Elisp plist LST."
  (let ((tail (car (last lst))))
   (concat
    "{"
    (cl-reduce
     (lambda (acc pair)
       (concat acc
               (clomacs-format-arg (car pair)) " "
               (clomacs-format-arg (cdr pair))
               (if (eq pair tail) "" " ")))
     lst
     :initial-value "")
    "}")))

(defun clomacs-format-arg (a)
  "Format Clojure representation of Elisp argument."
  (cond
   ((numberp a) (number-to-string a))
   ((stringp a) (clomacs-add-quotes a))
   ((booleanp a) (if a "true" "false"))
   ((clomacs-plist-p a) (clomacs-plist-to-map a))
   ((and (listp a) (equal (car a) 'quote))
    (concat "'" (clomacs-force-symbol-name
                 (cadr a))))
   ((symbolp a) (clomacs-force-symbol-name a))
   (t (replace-regexp-in-string
       "\\\\." "." (format "'%S" a)))))

(defvar clomacs-possible-return-types
  (list :string
        :int
        :number
        :list
        :char
        :vector))

(defun clomacs-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("clomacs-defun\\b" . font-lock-keyword-face)
     ("clomacs-def\\b" . font-lock-keyword-face))))

(defun clomacs-force-symbol-name (some-symbol)
  "Return lisp symbol SOME-SYMBOL as a string at all costs!"
  (mapconcat 'char-to-string
             (string-to-list (symbol-name some-symbol)) ""))

(eval-after-load "clomacs"
  (lambda ()
    (clomacs-highlight-initialize)

    ;; Should be last `clomacs-defun'
    (clomacs-defun clomacs--doc
                   clojure.repl/doc
                   :return-value :stdout)

    (defun clomacs-doc (x)
      (if (clomacs-get-connection)
          (clomacs--doc x)))))

(defun clomacs-get-doc (doc cl-entity-name)
  "Form the emacs-lisp side entity docstring.
DOC - user-defined docsting.
CL-ENTITY-NAME - clojure side entity name.
CL-ENTITY-TYPE - \"value\" or \"function\""
  (if doc doc
    (concat "Wrapped clojure entity:"
            (let ((cl-entity-doc (when (fboundp 'clomacs-doc)
                                   (clomacs-doc cl-entity-name))))
              (if cl-entity-doc (concat "\n" cl-entity-doc)
                (clomacs-force-symbol-name cl-entity-name))))))

(defun clomacs-ensure-nrepl-run (&optional lib-name)
  "Ensure nrepl is running."
  (when  clomacs-verify-nrepl-on-call
    (unless (clomacs-get-connection lib-name)
      (if clomacs-autoload-nrepl-on-call
          ;; Raise up the world - sync nrepl launch
          (clomacs-launch-nrepl lib-name t)
        (error
         (concat "CIDER is not launched!"))))))

(defun clomacs-get-result (result value type namespace)
  "Parse result of clojure code evaluation from CIDER.
Handle errors. Handle difference between CIDER versions."
  (let ((val-new (cond
                  ((equal value :value) "value")
                  ((equal value :stdout) "out"))))
    (if (or (plist-get result :stderr)
            (nrepl-dict-get result "err"))
        (error (or (plist-get result :stderr)
                   (nrepl-dict-get result "err")))
      (clomacs-format-result
       (let ((essence-result (or
                              (plist-get result value)
                              (nrepl-dict-get result val-new))))
         (if (and namespace (equal value :value))
             (substring essence-result 3)
           essence-result))
       type))))

(defun clomacs-add-quotes (str)
  (format "%S"
          (progn
            (set-text-properties 0 (length str) nil str)
            str)))

(cl-defun clomacs-prepare-vars (cl-entity-name
                                &key
                                (doc nil)
                                (return-type :string)
                                namespace)
  "Prepare intermediate variables for clomacs wrapper macros."
  (cl-assert (and return-type
                  (or (functionp return-type)
                      (member return-type clomacs-possible-return-types)))
             t
             (format (concat "Wrong return-type %s! See  C-h v "
                             "clomacs-possible-return-types")
                     (clomacs-force-symbol-name return-type)))
  (let* ((doc (clomacs-get-doc doc cl-entity-name))
         (cl-entity-name-str (clomacs-force-symbol-name cl-entity-name))
         (ns-slash-pos (string-match "/" cl-entity-name-str))
         (implicit-ns (if ns-slash-pos
                          (substring cl-entity-name-str 0 ns-slash-pos)))
         (namespace-str (if namespace
                            (clomacs-force-symbol-name namespace)
                          implicit-ns))
         (cl-entity-full-name (if (and namespace (not implicit-ns))
                                  (concat namespace-str "/" cl-entity-name-str)
                                cl-entity-name-str)))
    (list doc namespace-str cl-entity-full-name)))

;;;###autoload
(cl-defmacro clomacs-def (el-entity-name
                          cl-entity-name
                          &key
                          (doc nil)
                          (type :string)
                          lib-name
                          namespace)
  "Wrap CL-ENTITY-NAME, evaluated on clojure side by EL-ENTITY-NAME.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default)."
  (cl-multiple-value-bind
      (doc namespace-str cl-entity-full-name)
      (clomacs-prepare-vars cl-entity-name
                            :doc doc
                            :namespace namespace)
    `(defvar ,el-entity-name
       (progn
         (clomacs-ensure-nrepl-run ,lib-name)
         (let* ((connection (clomacs-get-connection ,lib-name))
                (result
                 (nrepl-sync-request:eval
                  (concat
                   (if ',namespace
                       (concat "(require '" ',namespace-str ") ") "")
                   ',cl-entity-full-name)
                  connection)))
           (clomacs-get-result result :value ',type ',namespace)))
       ,doc)))

;;;###autoload
(cl-defmacro clomacs-defun (el-func-name
                            cl-func-name
                            &key
                            (call-type :sync)
                            (callback nil)
                            (doc nil)
                            (interactive nil)
                            (return-type :string)
                            (return-value :value)
                            lib-name
                            namespace
                            (httpd-starter nil))
  "Wrap CL-FUNC-NAME, evaluated on clojure side by EL-FUNC-NAME.
CALL-TYPE - call Clojure side :sync or :async.
CALLBACK - callback function for :async CALL-TYPE case.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
INTERACTIVE - when defined and is a boolean `t` mark function (interactive),
if not boolean - insert interactive value into the function beginning as is.
RETURN-TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default).
RETURN-VALUE may be :value or :stdout (:value by default).
HTTPD-STARTER - in the case Clojure side code needs to call Elisp side code,
http-server should be started to pass http requests from Clojure REPL
to Emacs. This parameter is Elisp function to do it. Such function can
looks like `clomacs-httpd-start'."
  (cl-multiple-value-bind
      (doc namespace-str cl-entity-full-name)
      (clomacs-prepare-vars cl-func-name
                            :doc doc
                            :namespace namespace)
    `(defun ,el-func-name (&rest attributes)
       ,doc
       ,(if interactive
            (if (booleanp interactive)
                '(interactive)
              interactive))
       (clomacs-ensure-nrepl-run ,lib-name)
       (when (and (functionp ,httpd-starter)
                  (not (process-status "httpd")))
         (funcall ,httpd-starter))
       (let* ((attrs ""))
         (dolist (a attributes)
           (setq attrs (concat attrs " "
                               (clomacs-format-arg a))))
         (let* ((connection (clomacs-get-connection ,lib-name))
                (request (concat
                          (if ',namespace
                              (format  "(require '%s) " ',namespace-str) "")
                          (format
                           "(do (set! *print-length* %s)
                              (%s %s))"
                           ,(number-to-string clomacs-print-length)
                           ',cl-entity-full-name
                           attrs))))
           (if (equal ,call-type :async)
               ;; async
               (nrepl-request:eval
                request
                (lambda (result)
                  (if ,callback
                      (let ((el-result (clomacs-get-result
                                        result
                                        ,return-value ',return-type ',namespace)))
                        (if el-result
                            (,callback el-result))
                        (if clomacs-restore-print-length
                            (cider-repl-set-config)))))
                connection)
             ;; sync
             (let ((el-result (clomacs-get-result
                               (nrepl-sync-request:eval
                                request
                                connection)
                               ,return-value ',return-type ',namespace)))
               (if clomacs-restore-print-length
                   (cider-repl-set-config))
               el-result)))))))

(defun clomacs-load-file (file-path)
  "Sync and straightforward load clojure file."
  (let* ((connection (cider-current-connection))
         (session (clomacs-get-session connection)))
    (nrepl-sync-request:eval
     (with-temp-buffer
       (insert-file-contents file-path)
       (buffer-string))
     connection
     session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs http server

(defun clomacs-eval-elisp (string)
  "Evaluate elisp code stored in a STRING."
  (eval (car (read-from-string string))))

(defservlet* execute text/plain (elisp)
  (insert (format
           "%s"
           (clomacs-eval-elisp elisp))))

(clomacs-defun clomacs-require
               clojure.core/require)

(clomacs-defun clomacs-set-emacs-connection
               clomacs/set-emacs-connection)

(clomacs-defun clomacs-get-emacs-connection
               clomacs/get-emacs-connection)

(clomacs-defun clomacs-close-emacs-connection
               clomacs/close-emacs-connection)

(defun clomacs-httpd-start ()
  "Start Emacs http server and set host and port on Clojure side."
  (let ((httpd-port clomacs-httpd-port))
    (clomacs-require `'clomacs)
    (clomacs-set-emacs-connection "localhost" httpd-port)
    (httpd-start)))

(defun clomacs-httpd-stop ()
  "Stop Emacs http server and reset host and port on Clojure side."
  (clomacs-require `'clomacs)
  (clomacs-close-emacs-connection)
  (httpd-stop))

(provide 'clomacs)

;;; clomacs.el ends here
