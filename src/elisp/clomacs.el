;;; clomacs.el --- Simplifies Emacs Lisp interaction with Clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.2
;; Package-Requires: ((Emacs "24.3") (cider "0.11"))

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

(defun clomacs-launch-nrepl (library &optional sync)
  (let* ((starting-msg (format
                        "Starting nREPL server for %s..."
                        (propertize (or library "current-buffer")
                                    'face 'font-lock-keyword-face)))
         (lib-file (if library (find-library-name library)))
         (is-opened (if lib-file (find-buffer-visiting lib-file)))
         (lib-buff (or is-opened
                       (if lib-file
                           (find-file-noselect lib-file))))
         (old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
    ;; simple run lein
    (if lib-buff
        (with-current-buffer lib-buff
          (cider-jack-in))
      (cider-jack-in))
    (message starting-msg)
    (if sync
        (let ((old-cider-repl-pop cider-repl-pop-to-buffer-on-connect))
          (setq cider-repl-pop-to-buffer-on-connect nil)
          (while (not (cider-connected-p))
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
  (assert return-type)
  (let ((return-string (clomacs-strip-string raw-string)))
    (cond
     ((functionp return-type) (funcall return-type raw-string))
     ((eq return-type :string) return-string)
     ((eq return-type :int) (string-to-int return-string))
     ((eq return-type :number) (string-to-number return-string))
     ((eq return-type :list) (read raw-string))
     ((eq return-type :char) (string-to-char return-string))
     ((eq return-type :vector) (string-to-vector return-string)))))

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
   '(("clomacs-defun" . font-lock-keyword-face)
     ("clomacs-def" . font-lock-keyword-face))))

(defun clomacs-force-symbol-name (some-symbol)
  "Return lisp symbol SOME-SYMBOL as a string at all costs!"
  (mapconcat 'char-to-string
             (string-to-list (symbol-name some-symbol)) ""))

(defun clomacs-doc (x)) ; dummy definition, real definition is below.

(eval-after-load "clomacs"
  '(progn
     ;; Should be last `clomacs-defun'
     (clomacs-defun clomacs--doc
                    clojure.repl/doc
                    :return-value :stdout)
     (defun clomacs-doc (x)
       (if (cider-connected-p)
           (clomacs--doc x)))
     (clomacs-highlight-initialize)))

(defun clomacs-get-doc (doc cl-entity-name)
  "Form the emacs-lisp side entity docstring.
DOC - user-defined docsting.
CL-ENTITY-NAME - clojure side entity name.
CL-ENTITY-TYPE - \"value\" or \"function\""
  (if doc doc
    (concat "Wrapped clojure entity:"
            (let ((cl-entity-doc (clomacs-doc cl-entity-name)))
              (if cl-entity-doc (concat "\n" cl-entity-doc)
                (clomacs-force-symbol-name cl-entity-name))))))

(defun clomacs-ensure-nrepl-run (&optional lib-name)
  "Ensure nrepl is running."
  (when  clomacs-verify-nrepl-on-call
    (unless (cider-connected-p)
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
                                (return-value :value)
                                lib-name
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
                            :return-type type
                            :lib-name lib-name
                            :namespace namespace)
    `(defvar ,el-entity-name
       (progn
         (clomacs-ensure-nrepl-run ,lib-name)
         (let ((result
                (nrepl-sync-request:eval
                 (concat
                  (if ',namespace
                      (concat "(require '" ',namespace-str ") ") "")
                  ',cl-entity-full-name)
                 (cider-current-connection)
                 (cider-current-session))))
           (clomacs-get-result result :value ',type ',namespace)))
       ,doc)))

(cl-defmacro clomacs-defun (el-func-name
                            cl-func-name
                            &optional &key
                            (doc nil)
                            (return-type :string)
                            (return-value :value)
                            lib-name
                            namespace)
  "Wrap CL-FUNC-NAME, evaluated on clojure side by EL-FUNC-NAME.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
RETURN-TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default).
RETURN-VALUE may be :value or :stdout (:value by default)."
  (cl-multiple-value-bind
      (doc namespace-str cl-entity-full-name)
      (clomacs-prepare-vars cl-func-name
                            :doc doc
                            :return-type return-type
                            :lib-name lib-name
                            :namespace namespace)
    `(defun ,el-func-name (&rest attributes)
       ,doc
       (clomacs-ensure-nrepl-run ,lib-name)
       (let* ((attrs ""))
         (dolist (a attributes)
           (setq attrs (concat attrs " "
                               (cond
                                ((numberp a) (number-to-string a))
                                ((stringp a) (clomacs-add-quotes a))
                                ((booleanp a) (if a "true" "false"))
                                ((and (listp a) (equal (car a) 'quote))
                                 (concat "'" (clomacs-force-symbol-name
                                              (cadr a))))
                                ((symbolp a) (clomacs-force-symbol-name a))
                                (t (replace-regexp-in-string
                                    "\\\\." "." (format "'%S" a)))))))
         (let ((result
                (nrepl-sync-request:eval
                 (concat
                  (if ',namespace
                      (concat "(require '" ',namespace-str ") ") "")
                  "(" ',cl-entity-full-name attrs ")")
                 (cider-current-connection)
                 (cider-current-session))))
           (clomacs-get-result
            result ,return-value ',return-type ',namespace))))))

(defun clomacs-load-file (file-path)
  "Sync and straightforward load clojure file."
  (nrepl-send-string-sync
   (with-temp-buffer
     (insert-file-contents file-path)
     (buffer-string))))

(provide 'clomacs)

;;; clomacs.el ends here
