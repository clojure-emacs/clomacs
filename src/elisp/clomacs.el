;;; clomacs.el --- Simplifies emacs lisp interaction with clojure. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/clojure-emacs/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.2

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

;;; Commentary

;; `clomacs-defun' - core clojure to elisp function wrapper.
;;
;; See README.md for detail description.


(require 'cl-lib)
(require 'cider)

(defvar clomacs-verify-nrepl-on-call t)
(defvar clomacs-autoload-nrepl-on-call t)

(defun clomacs-is-session-here (nrepl-connection-buffer)
  "Return nrepl session for current nrepl-connection buffer.
Return nil if there is no such buffer or session in it."
  (save-excursion
    (and (buffer-live-p nrepl-connection-buffer)
         (progn
           (set-buffer nrepl-connection-buffer)
           nrepl-session))))

(defun clomacs-get-connection (&optional library)
  "Return t if nrepl process is running, nil otherwise."
  (let ((connections cider-connections))
   (if (and (not library) (> (length connections) 0))
       (nrepl-current-session)
     (let ((library (or library "localhost")))
       (and
        (> (length connections) 0)
        (reduce
         (lambda (x y) (or x y))
         (mapcar
          '(lambda (x)
             (let ((repl-project-name
                    (cadr (split-string (buffer-name x) " "))))
               (if (equal (substring repl-project-name
                                     0
                                     (- (length repl-project-name) 1) )
                          library)
                   x)))
          cider-connections)))))))

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
       (if (clomacs-get-connection)
           (clomacs--doc x)))
     (clomacs-highlight-initialize)))

(defun clomacs-get-doc (doc cl-entity-name cl-entity-type)
  "Form the emacs-lisp side entity docstring.
DOC - user-defined docsting.
CL-ENTITY-NAME - clojure side entity name.
CL-ENTITY-TYPE - \"value\" or \"function\""
  (if doc doc
    (concat (format "Wrapped clojure %s: " cl-entity-type)
            (let ((cl-entity-doc (clomacs-doc cl-entity-name)))
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
         (concat "Nrepl is not launched!"))))))

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

(cl-defmacro clomacs-def (el-entity-name
                          cl-entity-name
                          &optional &key
                          (doc nil)
                          (type :string)
                          lib-name
                          namespace)
  (let ((doc (clomacs-get-doc doc cl-entity-name "value")))
    `(defvar ,el-entity-name
       (progn
         (clomacs-ensure-nrepl-run ,lib-name)
         (let ((result
                (nrepl-send-string-sync
                 (concat
                  (if ',namespace
                      (concat
                       "(require '"
                       (clomacs-force-symbol-name ',namespace) ") "))
                  (clomacs-force-symbol-name ',cl-entity-name)))))
           (clomacs-get-result result :value ',type ',namespace)))
       ,doc)))

(defun clomacs-get-current-connection-buffer (lib-name)
  (format nrepl-repl-buffer-name-template
          (concat " " lib-name)))

(cl-defmacro clomacs-defun (el-func-name
                            cl-func-name
                            &optional &key
                            (doc nil)
                            (return-type :string)
                            (return-value :value)
                            lib-name
                            namespace)
  "Wrap CL-FUNC-NAME, evaluated on clojure side by EL-FUNC-NAME.
DOC - optional elisp function docstring.
The RETURN-TYPE possible values are listed in the
CLOMACS-POSSIBLE-RETURN-TYPES, or it may be a function (:string by default).
The RETURN-VALUE may be :value or :stdout (:value by default)."
  (if (and return-type
           (not (functionp return-type))
           (not (member return-type clomacs-possible-return-types)))
      (error "Wrong return-type %s! See  C-h v clomacs-possible-return-types"
             (clomacs-force-symbol-name return-type)))
  (let ((doc (clomacs-get-doc doc cl-func-name "function")))
    `(defun ,el-func-name (&rest attributes)
       ,doc
       (clomacs-ensure-nrepl-run ,lib-name)
       (let* ((attrs "")
              (cl-func-name-str (clomacs-force-symbol-name ',cl-func-name))
              (namespace-str (clomacs-force-symbol-name ',namespace))
              (ns-slash-pos (string-match "/" cl-func-name-str))
              (implicit-ns (if ns-slash-pos
                               (substring cl-func-name-str 0 ns-slash-pos))))
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
                      (concat "(require '" namespace-str ") "))
                  "(" (if (and ',namespace (not implicit-ns))
                          (concat namespace-str "/"))
                  cl-func-name-str attrs ")")
                 (clomacs-get-connection ,lib-name)
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
