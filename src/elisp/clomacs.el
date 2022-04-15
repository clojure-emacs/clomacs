;;; clomacs.el --- Simplifies Emacs Lisp interaction with Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2020 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.5
;; Package-Requires: ((emacs "24.3") (cider "0.22.1") (s "1.12.0") (simple-httpd "1.4.6") (dash "2.19.1"))

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
;; Elisp to Clojure calls helper functions:
;; `clomacs-create-httpd-start' - package-specific httpd connection setup.
;; `clomacs-create-httpd-stop' - package-specific httpd connection stop.
;;
;; See README.md for detailed description.


(require 'cl-lib)
(require 'find-func)
(require 'net-utils)
(require 'sesman)
(require 'cider)
(require 'clojure-mode)
(require 's)
(require 'simple-httpd)
(require 'dash)

(defcustom clomacs-auto-start-nrepl t
  "When t starts nREPL if Clojure wrapped function is called without nREPL."
  :group 'clomacs
  :type 'boolean)

(defcustom clomacs-httpd-default-port 8080
  "Http port to listen for requests from Clojure side."
  :group 'clomacs
  :type 'integer)

(defcustom clomacs-httpd-port-scan-limit 100
  "Available Http ports scan range limit."
  :group 'clomacs
  :type 'integer)

(eval-and-compile
  (defcustom clomacs-print-length 100000
    "Value for *print-length* set during `clomacs-defun' macros evaluation.
Restricts list length passed from Clojure to Emacs lisp.
Set `nil' for unlimited list length."
    :group 'clomacs
    :type 'integer))

(defcustom clomacs-allow-other-repl nil
  "When t allow use any CIDER nREPL not only library dedicated nREPL.
`nil' by default."
  :group 'clomacs
  :type 'boolean)

(defun clomacs-get-dir (repl-info)
  (if repl-info
      (file-name-nondirectory
       (car (split-string repl-info ":")))))

(eval-and-compile
  (defun clomacs-search-connection (project-name)
    "Search nREPL connection buffer.
E.g. if you want to find \"*cider-repl clomacs-20160419.258*\" you shold pass
REPL-BUFFER-PROJECT-NAME \"clomacs\"."
    (if project-name
        (let ((result nil))
          (maphash
           (lambda (k v)
             (let ((current-project-dir (clomacs-get-dir (cdr k))))
               (if (and current-project-dir
                        (or (s-contains? project-name current-project-dir)
                            (s-contains? project-name (buffer-name (cadr v)))))
                   (setq result (cadr v)))))
           sesman-sessions-hashmap)
          result)
      (cider-current-connection))))

(eval-and-compile
  (defun clomacs-get-first-connection ()
    "Get any first CIDER session buffer."
    (-> (sesman-sessions 'CIDER) car cdr car))

  (defun clomacs-get-connection (&optional library)
    "Return buffer with nREPL process related to LIBRARY.
If LIBRARY is nil, attempts to use \"clomacs\", \"localhost\" or
any current connection.
If can't find any nREPL process return nil."
    (if library
        (or (clomacs-search-connection library)
            (if (or noninteractive clomacs-allow-other-repl)
                (or (cider-current-repl)
                    (clomacs-get-first-connection))))
      ;; No `:lib-name' parameter or nil.
      (or (clomacs-search-connection "clomacs")
          (cider-current-repl)
          (clomacs-get-first-connection)))))

(defun clomacs-get-session (connection)
  "Return current session for this CONNECTION."
  (cl-assert connection)
  (with-current-buffer
    (set-buffer connection)
    (cider-nrepl-eval-session)))

(defun clomacs-jack-in-clj (params wrapped-eval attributes nrepl-ready-callback)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd."
  (let ((params (thread-first params
                  (cider--update-project-dir)
                  (cider--check-existing-session)
                  (cider--update-jack-in-cmd)))
        (old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buff)
       (prog1 (cider-connect-sibling-clj params server-buff)
         (let ((eval-result (if wrapped-eval
                                (apply wrapped-eval attributes))))
           (when nrepl-ready-callback
             (funcall nrepl-ready-callback eval-result)
             nil))
         (setq cider-repl-pop-to-buffer-on-connect old-cider-repl-pop))))))

(defun clomacs-jack-in-cljs (params wrapped-eval attributes nrepl-ready-callback)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. Node, Figwheel, etc)."
  (let ((cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
        (cider-jack-in-lein-plugins (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
        (cider-jack-in-nrepl-middlewares (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares))
        (orig-buffer (current-buffer))
        ;; cider--update-jack-in-cmd relies indirectly on the above dynamic vars
        (params (thread-first params
                  (cider--update-project-dir)
                  (cider--check-existing-session)
                  (cider--update-jack-in-cmd)))
        (old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buff)
       (prog1 (with-current-buffer orig-buffer
                (cider-connect-sibling-cljs params server-buff))
         (let ((eval-result (if wrapped-eval
                                (apply wrapped-eval attributes))))
           (when nrepl-ready-callback
             (funcall nrepl-ready-callback eval-result)
             nil))
         (setq cider-repl-pop-to-buffer-on-connect old-cider-repl-pop))))))

(defun clomacs-launch-nrepl (lib-name
                             wrapped-eval
                             attributes
                             nrepl-ready-callback
                             backend)
  (let* ((starting-msg (format
                        "Starting nREPL server for %s..."
                        (propertize (or lib-name "current-buffer")
                                    'face 'font-lock-keyword-face)))
         (project-dir (if-let* ((lib-file (if lib-name
                                              (find-library-name lib-name))))
                          (clojure-project-dir
                           (file-name-directory lib-file))))
         (params (if project-dir (list :project-dir project-dir))))
    ;; simple run lein
    (pcase backend
      (:clj (clomacs-jack-in-clj params
                                 wrapped-eval
                                 attributes
                                 nrepl-ready-callback))
      (:cljs (clomacs-jack-in-cljs params
                                   wrapped-eval
                                   attributes
                                   nrepl-ready-callback))
      (_ (error "Unknown backend %s" backend)))
    (message starting-msg))
  nil)

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

(defun clomacs-string-to-boolean (s)
  (not (or (not s)
           (equal s "nil")
           (equal s "false"))))

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
         ((eq return-type :boolean) (clomacs-string-to-boolean return-string))
         ((eq return-type :list) (read raw-string))
         ((eq return-type :char) (string-to-char return-string))
         ((eq return-type :vector) (read return-string))
         ((eq return-type :eval) (eval (read (read raw-string))))))))

(declare clomacs-format-arg)

(defun clomacs-alist-p (object)
  "Return t if OBJECT is a alist, otherwise, return nil."
  (when (and (listp object)
             (car object)
             (listp (car object))
             (not (listp (cdr (car object)))))
    t))

(defun clomacs-alist-to-map (lst)
  "Build string representation of Clojure map from Elisp alist LST."
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
   ((clomacs-alist-p a) (clomacs-alist-to-map a))
   ((and (listp a) (equal (car a) 'quote))
    (concat "'" (clomacs-force-symbol-name
                 (cadr a))))
   ((symbolp a) (clomacs-force-symbol-name a))
   (t (replace-regexp-in-string
       "\\\\." "." (format "'%S" a)))))

(eval-and-compile
  (defvar clomacs-possible-return-types
    (list :string
          :int
          :number
          :list
          :char
          :vector
          :eval)))

(defun clomacs-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("clomacs-defun\\b" . font-lock-keyword-face)
     ("clomacs-def\\b" . font-lock-keyword-face)
     ("clomacs-with-nrepl\\b" . font-lock-keyword-face))))

(eval-and-compile
  (defun clomacs-force-symbol-name (some-symbol)
    "Return lisp symbol SOME-SYMBOL as a string at all costs!"
    (mapconcat 'char-to-string
               (string-to-list (symbol-name some-symbol)) "")))

(eval-after-load "clomacs"
  (lambda ()
    (clomacs-highlight-initialize)))

(eval-and-compile
  (defun clomacs-get-doc (doc cl-entity-name)
    "Form the emacs-lisp side entity docstring.
DOC - user-defined docsting.
CL-ENTITY-NAME - clojure side entity name.
CL-ENTITY-TYPE - \"value\" or \"function\""
    (if doc doc
      (format "Wrapped clojure entity: %s%s"
              cl-entity-name
              (let ((cl-entity-doc (if (clomacs-get-connection)
                                       (condition-case _
                                           (nrepl-dict-get
                                            (cider-var-info
                                             (symbol-name cl-entity-name)) "doc")
                                         (error nil)))))
                (if cl-entity-doc
                    (concat "\n" cl-entity-doc)
                  ""))))))

(defun clomacs-ensure-nrepl-run (lib-name
                                 wrapped-eval
                                 attributes
                                 nrepl-ready-callback
                                 backend)
  "Ensure nREPL is running."
  (if (clomacs-get-connection lib-name)
      (if wrapped-eval
          (apply wrapped-eval attributes))
    (if clomacs-auto-start-nrepl
        ;; Raise up the world - nrepl launch
        (clomacs-launch-nrepl lib-name
                              wrapped-eval
                              attributes
                              nrepl-ready-callback
                              backend)
      (error
       (concat "CIDER is not launched!")))))

(cl-defun clomacs-with-nrepl (lib-name
                              wrapped-eval
                              &key params
                              (backend :clj))
  "Used to call lambda with multiple Elisp to Clojure wrapped functions.
LIB-NAME - Elisp library name used in end-user .emacs config by `require'.
WRAPPED-EVAL is a lambda that can contain any `clomacs-defun' created
functions. Since nREPL can be not launched yet, this lambda is evaled as
a callback after nREPL started.
PARAMS is a list of the values for parameters of preceding lambda."
  (declare (indent 1))
  (clomacs-ensure-nrepl-run lib-name wrapped-eval params nil backend))

(defun clomacs-get-result (result value type namespace)
  "Parse result of clojure code evaluation from CIDER."
  (if (nrepl-dict-get result "err")
      (error (nrepl-dict-get result "err"))
    (cl-labels ((get-value () (clomacs-format-result
                               (let ((essence-result
                                      (nrepl-dict-get result "value")))
                                 (if (and namespace (equal value :value))
                                     (substring essence-result 3)
                                   essence-result))
                               type))
                (get-out () (clomacs-format-result
                             (nrepl-dict-get result "out")
                             type)))
      (cond
       ((equal value :value) (get-value))
       ((equal value :stdout) (get-out))
       ((equal value :both) (cons (get-out) (get-value)))))))

(defun clomacs-add-quotes (str)
  (format "%S"
          (progn
            (set-text-properties 0 (length str) nil str)
            str)))

(eval-and-compile
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
      (list doc namespace-str cl-entity-full-name))))

;;;###autoload
(cl-defmacro clomacs-def (el-entity-name
                          cl-entity-name
                          &key
                          (doc nil)
                          (type :string)
                          lib-name
                          namespace
                          nrepl-ready-callback
                          (backend :clj))
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
       (clomacs-ensure-nrepl-run
          ,lib-name
          (lambda ()
            (let* ((connection (clomacs-get-connection ,lib-name))
                   (result
                    (nrepl-sync-request:eval
                     (concat
                      (if ',namespace
                          (concat "(require '" ',namespace-str ") ") "")
                      ',cl-entity-full-name)
                     connection)))
              (clomacs-get-result result :value ',type ',namespace)))
          nil
          ,nrepl-ready-callback
          ,backend)
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
                            (httpd-starter nil)
                            nrepl-ready-callback
                            (backend :clj))
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
LIB-NAME - Elisp library name used in end-user .emacs config by `require'.
HTTPD-STARTER - in the case Clojure side code needs to call Elisp side code,
http-server should be started to pass http requests from Clojure REPL
to Emacs. This parameter is Elisp function to do it. Such function can
be created by `clomacs-create-httpd-start' macro.
NREPL-READY-CALLBACK in case of wrapped function called when nREPL not
started yet, a labmbda with one parameter - the result of wrapped function
evaluation can be added and executed."
  (cl-multiple-value-bind
      (doc namespace-str cl-entity-full-name)
      (clomacs-prepare-vars cl-func-name
                            ;; handle case when docstring made by (concat ...)
                            :doc (if (stringp doc) doc (eval doc))
                            :namespace namespace)
    `(defun ,el-func-name (&rest attributes)
       ,doc
       ,(if interactive
            (if (booleanp interactive)
                '(interactive)
              interactive))
       (clomacs-ensure-nrepl-run
        ,lib-name
        (lambda (&rest attributes)
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
                                           ,return-value
                                           ',return-type
                                           ',namespace)))
                           (if el-result
                               (,callback el-result)))))
                   connection)
                ;; sync
                (let ((el-result (clomacs-get-result
                                  (nrepl-sync-request:eval
                                   request
                                   connection)
                                  ,return-value ',return-type ',namespace)))
                  el-result)))))
        attributes
        ,nrepl-ready-callback
        ,backend))))

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

(defservlet* execute text/plain (fname elisp debug)
  (if (equal debug "true")
      (message "Clojure->Elisp function: %s\n  elisp: %s" fname elisp))
  (condition-case err
      (let ((result (clomacs-eval-elisp elisp)))
        (if result
            (insert (format "%s" result))))
    (error
     (message
      "%s\n  in wrapped Clojure->Elisp function: %s\n  elisp: %s"
      (error-message-string err)
      fname
      elisp))))

(defun clomacs-get-httpd-port ()
  "Search available port for httpd process."
  (let ((netstat-ouptut (shell-command-to-string
                         (concat netstat-program " -an")))
        (value)
        (i 0))
    (while (not value)
      (let ((port (+ clomacs-httpd-default-port i)))
        (setq i (+ i 1))
        (if (> i clomacs-httpd-port-scan-limit)
            (error (format "All ports from %d to %d are busy."
                           clomacs-httpd-default-port
                           port)))
        (if (not (s-contains? (number-to-string port) netstat-ouptut))
            (setq value port))))
    value))

(cl-defmacro clomacs-create-httpd-start (func-name &key lib-name)
  "Create lib-specific function FUNC-NAME, aimed to start Emacs httpd process.
LIB-NAME - Elisp library name used in end-user .emacs config by `require'.
The result function FUNC-NAME can be used as `clomacs-defun'
`:httpd-starter' parameter."
  `(defun ,func-name ()
     "Start Emacs http server and set host and port on Clojure side."
     (let ((httpd-port (clomacs-get-httpd-port))
           (lib-require (clomacs-defun ,(make-symbol
                                         (concat lib-name "-require"))
                                       clojure.core/require
                                       :lib-name ,lib-name))
           (set-connection (clomacs-defun ,(make-symbol
                                            (concat lib-name
                                                    "-set-connection"))
                                          clomacs/set-emacs-connection
                                          :lib-name ,lib-name)))
       (funcall lib-require `'clomacs)
       (funcall set-connection "localhost" httpd-port)
       (httpd-start))))

(cl-defmacro clomacs-create-httpd-stop (func-name &key lib-name)
  "Create lib-specific function FUNC-NAME, aimed to stop Emacs httpd process.
LIB-NAME - Elisp library name used in end-user .emacs config by `require'."
  `(defun ,func-name ()
     "Stop Emacs http server and reset host and port on Clojure side."
     (let ((lib-require (clomacs-defun ,(make-symbol
                                         (concat lib-name "-require"))
                                       clojure.core/require
                                       :lib-name ,lib-name))
           (close-connection (clomacs-defun ,(make-symbol
                                              (concat lib-name
                                                      "-close-connection"))
                                            clomacs/close-emacs-connection
                                            :lib-name ,lib-name)))
       (when (clomacs-get-connection ,lib-name)
         (funcall lib-require `'clomacs)
         (funcall close-connection))
       (httpd-stop))))

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
  (let ((httpd-port (clomacs-get-httpd-port)))
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
