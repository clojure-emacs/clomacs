;;; clomacs.el --- Simplifies emacs lisp interaction with clojure.

;; Copyright (C) 2013 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/clomacs
;; Keywords: clojure, interaction
;; Version: 0.0.1

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


(require 'cider)
(require 'clomacs-lib)

(defvar clomacs-nrepl-connrection-buffer-name (nrepl-connection-buffer-name))

(defvar clomacs-verify-nrepl-on-call t)

(defvar clomacs-autoload-nrepl-on-call t)

(defvar clomacs-custom-libs-loaded-list nil
  "A property list, contains the list of the libraries names already loaded
to the repl and associated with the every library lists of namespaces.")

(defvar clomacs-clojure-offline-file "clomacs.clj"
  "Clojure-offline src helper file name.")

(defvar clomacs-is-initialized nil
  "When nil `clomacs-clojure-offline-file' is not loaded yet, t otherwise.")

(defun clomacs-mark-uninitialized ()
  (setq clomacs-is-initialized nil)
  (setq clomacs-custom-libs-loaded-list nil))

(add-hook 'nrepl-connected-hook 'clomacs-mark-uninitialized)
(add-hook 'nrepl-disconnected-hook 'clomacs-mark-uninitialized)

(eval-and-compile
  (defvar clomacs-elisp-path
    (let ((path (or (locate-library "clomacs") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the clomacs elisp code."))

(defun clomacs-is-nrepl-runnig ()
  "Return t if nrepl process is running, nil otherwise."
  (let ((ncb (get-buffer clomacs-nrepl-connrection-buffer-name)))
    (save-excursion
      (if (and (buffer-live-p ncb)
               (progn
                 (set-buffer ncb)
                 nrepl-session))
          t nil))))

(defun clomacs-launch-nrepl (&optional clojure-side-file sync)
  (let ((starting-msg "Starting nREPL server..."))
    (save-excursion
      (let ((this-buffer (current-buffer))
            (jack-file (find-file-in-load-path clojure-side-file))
            (jack-buffer nil)
            (opened nil))
        (when jack-file
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (buffer-file-name buffer)
                         (buffer-live-p buffer)
                         (equal (downcase (buffer-file-name buffer))
                                (downcase jack-file)))
                (setq opened t))))
          (setq jack-buffer (find-file-noselect jack-file))
          (set-buffer jack-buffer))
        ;; simple run lein
        (cider-jack-in)
        (if (and jack-buffer
                 (not opened))
            (kill-buffer jack-buffer))
        (if sync
            (while (not (clomacs-is-nrepl-runnig))
              (sleep-for 0.1)
              (message starting-msg)))))
    (message starting-msg)))

(defun clomacs-ensure-nrepl-runnig (&optional clojure-side-file sync)
  "Ensures nrepl is runnig.
If not, launch it, return nil. Return t otherwise."
  (interactive)
  (let ((is-running (clomacs-is-nrepl-runnig)))
    (when (not is-running)
      (clomacs-launch-nrepl clojure-side-file sync))
    is-running))

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

(defun clomacs-doc (x)) ; dummy definition, real definition is below.

(eval-after-load "clomacs"
  '(progn
     ;; Should be last `clomacs-defun'
     (clomacs-defun clomacs--doc
                    clojure.repl/doc
                    :return-value :stdout)
     (defun clomacs-doc (x)
       (if (clomacs-is-nrepl-runnig)
           (clomacs--doc x)))))

(defun clomacs-get-doc (doc cl-entity-name cl-entity-type)
  "Form the emacs-lisp side entity docstring.
`doc' - user-defined docsting.
`cl-entity-name' - clojure side entity name.
`cl-entity-type' - \"value\" or \"function\""
  (if doc doc
    (concat (format "Wrapped clojure %s: " cl-entity-type)
            (let ((cl-entity-doc (clomacs-doc cl-entity-name)))
              (if cl-entity-doc (concat "\n" cl-entity-doc)
                (force-symbol-name cl-entity-name))))))

(defun clomacs-nrepl-verification ()
  "Verify nrepl is running and clomacs is initialized on wrapped entity call."
  (when  clomacs-verify-nrepl-on-call
    (unless (clomacs-is-nrepl-runnig)
      (clomacs-mark-uninitialized)
      (if clomacs-autoload-nrepl-on-call
          ;; Raise up the world
          (progn
            (clomacs-launch-nrepl nil t) ; sync nrepl launch.
            (clomacs-init))
        (error
         (concat "Nrepl is not launched! You can launch it via "
                 "M-x clomacs-ensure-nrepl-runnig"))))))

(defmacro* clomacs-def (el-entity-name
                        cl-entity-name
                        &optional &key
                        (doc nil)
                        (type :string)
                        lib-name
                        namespace)
  (let ((doc (clomacs-get-doc doc cl-entity-name "value")))
    `(defvar ,el-entity-name
       (progn
         (clomacs-nrepl-verification)
         (if (and ,lib-name ',namespace)
             (clomacs-load ,lib-name ',namespace))
         (let ((result
                (cider-read-and-eval
                 (concat (force-symbol-name ',cl-entity-name)))))
           (if (plist-get result :stderr)
               (error (plist-get result :stderr))
             (clomacs-format-result
              (plist-get result :value) ',type))))
       ,doc)))

(defmacro* clomacs-defun (el-func-name
                          cl-func-name
                          &optional &key
                          (doc nil)
                          (return-type :string)
                          (return-value :value)
                          lib-name
                          namespace)
  "Wrap `cl-func-name', evaluated on clojure side by `el-func-name'.
The `return-type' possible values are listed in the
`clomacs-possible-return-types', or it may be a function (:string by default).
The `return-value' may be :value or :stdout (:value by default)"
  (if (and return-type
           (not (functionp return-type))
           (not (member return-type clomacs-possible-return-types)))
      (error "Wrong return-type %s! See  C-h v clomacs-possible-return-types"
             (force-symbol-name return-type)))
  (let ((doc (clomacs-get-doc doc cl-func-name "function")))
    `(defun ,el-func-name (&rest attributes)
       ,doc
       (clomacs-nrepl-verification)
       (if (and ,lib-name ',namespace)
           (clomacs-load ,lib-name ',namespace))
       (let ((attrs ""))
         (dolist (a attributes)
           (setq attrs (concat attrs " "
                               (cond
                                ((numberp a) (number-to-string a))
                                ((stringp a) (add-quotes a))
                                ((booleanp a) (if a "true" "false"))
                                ((and (listp a) (equal (car a) 'quote))
                                 (concat "'" (force-symbol-name (cadr a))))
                                ((symbolp a) (force-symbol-name a))
                                (t (replace-regexp-in-string
                                    "\\\\." "." (format "'%S" a)))))))
         (let ((result
                (cider-read-and-eval
                 (concat "(" (force-symbol-name ',cl-func-name) attrs ")"))))
           (if (plist-get result :stderr)
               (error (plist-get result :stderr))
             (clomacs-format-result
              (plist-get result ,return-value) ',return-type)))))))

(clomacs-defun clomacs-add-to-cp
               clomacs.clomacs/add-to-cp
               :return-value :stdout)

(clomacs-defun clomacs-print-cp
               clomacs.clomacs/print-cp
               :return-type :string
               :return-value :stdout)

(clomacs-defun clomacs-use
               clojure.core/use)

(clomacs-defun clomacs-import
               clojure.core/import)

(clomacs-defun clomacs-print
               clojure.core/print
               :return-type :string
               :return-value :stdout)

(defadvice nrepl-create-repl-buffer
  (after clomacs-nrepl-create-repl-buffer (process))
  "Hack to restore previous buffer after nrepl launched."
    (previous-buffer))
(ad-activate 'nrepl-create-repl-buffer)

(defun clomacs--find-clojure-offline-file ()
  "Return the full path to `clomacs-clojure-offline-file'."
  (find-file-in-load-path clomacs-clojure-offline-file))

(defun clomacs-init ()
  "Init clomacs clojure side via load clojure-offline lib."
  (if (clomacs-is-nrepl-runnig)
      (when (not clomacs-is-initialized)
        (add-to-list 'load-path
                     (concat-path clomacs-elisp-path ".." "clj" "clomacs"))
        (let ((clof (clomacs--find-clojure-offline-file)))
          (cider-load-file-core clof)
          (setq clomacs-is-initialized t)
          (clomacs-add-to-cp
           (file-name-directory (expand-file-name ".." clof)))))
    ;; nrepl is not running
    (progn
      (clomacs-mark-uninitialized)
      (error "Nrepl is not launched!"))))

(defun clomacs-find-project-file (lib-name)
  "Return the full path to project.clj file.
`lib-name' - is the name of the custom library's main *.el file, which is
loaded to the user's .emacs file via (require '...)."
  (let ((path (file-name-directory (locate-library lib-name))))
    (find-file-upwards "project.clj" path 2)))

(defvar clomacs-project-file
  (clomacs-find-project-file "clomacs"))

(clomacs-defun clomacs-load-project-dependences
               clomacs.clomacs/load-project-dependences
               :doc "Load dependences, listed in the project.clj.
E.g. this call is unnecessary and used for self-testing:
  (clomacs-load-project-dependences clomacs-project-file)")

(clomacs-defun clomacs-add-source-paths
               clomacs.clomacs/add-source-paths
               :doc "Add the list of the source paths to the classpath.")

(clomacs-defun clomacs-in-ns
               clojure.core/in-ns)

(clomacs-defun clomacs-set-offline
               clomacs.clomacs/set-offline)

(defun clomacs-load (lib-name namespace)
  "Load all *.jar deps, evaluate user's clojure side file, mark lib as loaded."
  (assert lib-name)
  (assert namespace)
  (clomacs-init)
  (let ((is-lib-added (member lib-name clomacs-custom-libs-loaded-list))
        (is-ns-added (member namespace
                             (lax-plist-get
                              clomacs-custom-libs-loaded-list lib-name))))
    (unless is-ns-added
      (unless is-lib-added
        (let ((project-file-path (clomacs-find-project-file lib-name)))
          ;; load all *.jar dependences from project.clj
          (clomacs-load-project-dependences project-file-path)
          ;; add clojure-side source file paths to classpath
          (clomacs-add-source-paths project-file-path)))
      ;; load new namespace
      (clomacs-use `',namespace)
      ;; save libs and according namespaces loaded states
      (setq clomacs-custom-libs-loaded-list
            (lax-plist-put clomacs-custom-libs-loaded-list
                           lib-name
                           (cons namespace
                                 (lax-plist-get
                                  clomacs-custom-libs-loaded-list
                                  lib-name)))))))

;; (clomacs-is-nrepl-runnig)
;; (clomacs-ensure-nrepl-runnig)
;; (clomacs-launch-nrepl)
;; (progn
;;   (clomacs-launch-nrepl nil t) ; sync nrepl launch.
;;   (clomacs-init))
;; (clomacs-init)
;; (clomacs-print-cp)


(provide 'clomacs)
