;;; clomacs.el --- Simplifies Emacs Lisp interaction with Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3") (cider "0.11"))

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

(defvar clomacs-verify-nrepl-on-call t)
(defvar clomacs-autoload-nrepl-on-call t)

(defun clomacs-search-connection (repl-buffer-project-name)
  "Search nREPL connection buffer.
E.g. if you want to find \"*cider-repl clomacs-20160419.258*\" you shold pass
REPL-BUFFER-PROJECT-NAME \"clomacs\"."
  (cl-reduce
   (lambda (x y) (or x y))
   (mapcar
    (lambda (x)
      (let ((this-repl
             (cadr (split-string (buffer-name (car cider-connections)) " "))))
        (if (string= repl-buffer-project-name
                     (substring this-repl
                                0
                                (length repl-buffer-project-name)))
            x)))
    cider-connections)))

(defun clomacs-get-connection (&optional library)
  "Return buffer with nREPL process related to LIBRARY.
If LIBRARY is nil, attempts to use \"clomacs\", \"localhost\" or
any current connection.
If can't find any nREPL process return nil."
  (if (> (length cider-connections) 0)
      (if library
          (clomacs-search-connection library)
        (or (clomacs-search-connection "clomacs")
            (clomacs-search-connection "localhost")
            (cider-current-connection)))))

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
          (cider-jack-in))
      (cider-jack-in))
    (message starting-msg)
    (if sync
        (let ((old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
          (setq cider-repl-pop-to-buffer-on-connect nil)
          (while (not (clomacs-get-connection library))
            (sleep-for 0.1)
            (message starting-msg))
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

(defun clomacs-format-result (raw-string return-type)
  "Format Elisp representation of Clojure evaluation result."
  (cl-assert return-type)
  (if raw-string
      (let ((return-string (clomacs-strip-string raw-string)))
        (cond
         ((functionp return-type) (funcall return-type raw-string))
         ((eq return-type :string) return-string)
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
  (concat "\"" str "\""))

(cl-defun clomacs-prepare-vars (cl-entity-name
                                &optional &key
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
         (namespace-str (clomacs-force-symbol-name namespace))
         (ns-slash-pos (string-match "/" cl-entity-name-str))
         (implicit-ns (if ns-slash-pos
                          (substring cl-entity-name-str 0 ns-slash-pos)))
         (cl-entity-full-name (if (and namespace (not implicit-ns))
                                  (concat namespace-str "/" cl-entity-name-str)
                                cl-entity-name-str)))
    (list doc namespace-str cl-entity-full-name)))

;;;###autoload
(cl-defmacro clomacs-def (el-entity-name
                          cl-entity-name
                          &optional &key
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
                (session (clomacs-get-session connection))
                (result
                 (nrepl-sync-request:eval
                  (concat
                   (if ',namespace
                       (concat "(require '" ',namespace-str ") ") "")
                   ',cl-entity-full-name)
                  connection
                  session)))
           (clomacs-get-result result :value ',type ',namespace)))
       ,doc)))

;;;###autoload
(cl-defmacro clomacs-defun (el-func-name
                            cl-func-name
                            &optional &key
                            (call-type :sync)
                            (callback nil)
                            (doc nil)
                            (return-type :string)
                            (return-value :value)
                            lib-name
                            namespace)
  "Wrap CL-FUNC-NAME, evaluated on clojure side by EL-FUNC-NAME.
CALL-TYPE - call Clojure side :sync or :async.
CALLBACK - callback function for :async CALL-TYPE case.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
RETURN-TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default).
RETURN-VALUE may be :value or :stdout (:value by default)."
  (cl-multiple-value-bind
      (doc namespace-str cl-entity-full-name)
      (clomacs-prepare-vars cl-func-name
                            :doc doc
                            :namespace namespace)
    `(defun ,el-func-name (&rest attributes)
       ,doc
       (clomacs-ensure-nrepl-run ,lib-name)
       (let* ((attrs ""))
         (dolist (a attributes)
           (setq attrs (concat attrs " "
                               (clomacs-format-arg a))))
         (let* ((connection (clomacs-get-connection ,lib-name))
                (session (clomacs-get-session connection))
                (request (concat
                          (if ',namespace
                              (concat "(require '" ',namespace-str ") ") "")
                          "(" ',cl-entity-full-name attrs ")")))
           (if (equal ,call-type :async)
               (nrepl-request:eval
                request
                (lambda (result)
                  (if ,callback
                      (let ((el-result (clomacs-get-result
                                        result
                                        ,return-value ',return-type ',namespace)))
                        (if el-result
                            (,callback el-result)))))
                connection
                session)
             (clomacs-get-result
              (nrepl-sync-request:eval
               request
               connection
               session)
              ,return-value ',return-type ',namespace)))))))

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

(provide 'clomacs)

;;; clomacs.el ends here
