;;; Commentary

;; `clomacs-ensure-nrepl-runnig' - Ensures nrepl is runnig. If not, launch it,
;; return nil. Return t otherwise.
;; `clomacs-load' Evaluate clojure side, run startup initialization
;; functions.
;; `clomacs-defun' - core clojure to elisp function wrapper.
;;
;; There are some requirements to run mixed elisp-clojure code.
;;
;; Obviously, all the elisp-side code should be loaded at this point:
;; 1. clomacs elisp code should be loaded:
;;    (require 'clomacs)
;; 2. custom elisp code should be loaded:
;;    (require '<custom>)
;;
;; The clojure-side code requires the following:
;; 1. nrepl must run:
;;    (clomacs-launch-nrepl) / (clomacs-ensure-nrepl-runnig)
;; 2. clomacs clojure-side code should be loaded:
;;    (clomacs-init)
;; 3. user's java/clojure custom *.jar libraries should be added to the
;;    CLASSPATH:
;;    (clomacs-load-project-dependences <custom>)
;; 4. user's custom clojure code should be added to the CLASSPATH:
;;    (clomacs-load <custom> <clojure-side-file>)
;;
;; So, the user of the mixed elisp-clojure lib wants to simple run elisp code
;; from the <custom> lib.  But at this point we have the unknown state: probably
;; no one of this terms are supplied, or supplied some of them (or even all of
;; them).
;;
;; The purpose of the `clomacs-defun' is to do all of this verification and
;; loading (if necessary), but just once per library.


(require 'nrepl)
(require 'clomacs-lib)

(defvar clomacs-nrepl-connrection-buffer-name (nrepl-connection-buffer-name))

(defvar clomacs-verify-nrepl-on-call t)

(defvar clomacs-autoload-nrepl-on-call t)

(defvar clomacs-custom-libs-loaded-list nil
  "Contains the list of the libraries names already loaded to the repl.")

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
     ((functionp return-type) (apply return-type (list return-string)))
     ((eq return-type :string) return-string)
     ((eq return-type :int) (string-to-int return-string))
     ((eq return-type :number) (string-to-number return-string))
     ((eq return-type :list) (string-to-list return-string))
     ((eq return-type :char) (string-to-char return-string))
     ((eq return-type :vector) (string-to-vector return-string)))))

(defvar clomacs-possible-return-types
  (list :string
        :int
        :number
        :list
        :char
        :vector))

(defmacro* clomacs-defun (el-func-name
                          cl-func-name
                          &optional
                          &key (doc nil)
                          return-type
                          return-value
                          lib-name
                          clojure-side-file)
  "Wrap `cl-func-name', evaluated on clojure side by `el-func-name'.
The `return-type' possible values are listed in the
`clomacs-possible-return-types', or it may be a function (:string by default).
The `return-value' may be :value or :stdout (:value by default)"
  (if (and return-type
           (not (functionp return-type))
           (not (member return-type clomacs-possible-return-types)))
      (error "Wrong return-type! See  C-h v clomacs-possible-return-types"))
  (let ((return-type (if return-type return-type :string))
        (return-value (if return-value return-value :value)))
    `(defun ,el-func-name (&rest attributes)
       ,doc
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
                      "M-x clomacs-ensure-nrepl-runnig")))))
       (if (and ,lib-name ,clojure-side-file)
           (clomacs-load ,lib-name ,clojure-side-file))
       (let ((attrs ""))
         (dolist (a attributes)
           (setq attrs (concat attrs " "
                               (cond
                                ((numberp a) (number-to-string a))
                                ((stringp a) (add-quotes a))
                                (t (concat (force-symbol-name a)))))))
         (let ((result
                (nrepl-eval
                 (concat "(" (force-symbol-name ',cl-func-name) attrs ")"))))
           (if (plist-get result :stderr)
               (error (plist-get result :stderr))
             (clomacs-format-result
              (plist-get result ,return-value) ',return-type)))))))

(defadvice nrepl-create-repl-buffer
  (after clomacs-nrepl-create-repl-buffer (process))
  "Hack to restore previous buffer after nrepl launched."
    (previous-buffer))
(ad-activate 'nrepl-create-repl-buffer)

(defun clomacs-launch-nrepl (&optional clojure-side-file sync)
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
      (nrepl-jack-in)
      (if (and jack-buffer
               (not opened))
          (kill-buffer jack-buffer))
      (if sync
          (while (not (clomacs-is-nrepl-runnig))
            (sleep-for 0.1)
            (message "Starting nREPL server...")))))
  (message "Starting nREPL server..."))

(defun clomacs-ensure-nrepl-runnig (&optional clojure-side-file sync)
  "Ensures nrepl is runnig.
If not, launch it, return nil. Return t otherwise."
  (interactive)
  (let ((is-running (clomacs-is-nrepl-runnig)))
    (when (not is-running)
      (clomacs-launch-nrepl clojure-side-file sync))
    is-running))

(clomacs-defun clomacs-add-to-cp clomacs.clomacs/add-to-cp)

(clomacs-defun clomacs-print-cp
               clomacs.clomacs/print-cp :string :stdout)

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
          (nrepl-load-file-core clof)
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
This call is unnecessary and used for self-testing:
  (clomacs-load-project-dependences clomacs-project-file)")

(defun clomacs-load (lib-name clojure-side-file)
  "Load all *.jar deps, evaluate user's clojure side file, mark lib as loaded."
  (assert lib-name)
  (assert clojure-side-file)
  (clomacs-init)
  (when (not (member lib-name clomacs-custom-libs-loaded-list))
    ;; load all *.jar dependences from project.clj
    (clomacs-load-project-dependences (clomacs-find-project-file lib-name))
    (let ((to-load (find-file-in-load-path clojure-side-file)))
      ;; add clojure-side files to classpath
      (clomacs-add-to-cp (file-name-directory (expand-file-name ".." to-load)))
      ;; load clojure-side file
      (nrepl-load-file to-load))
    (add-to-list 'clomacs-custom-libs-loaded-list lib-name)))

;; (clomacs-is-nrepl-runnig)
;; (clomacs-ensure-nrepl-runnig)
;; (clomacs-launch-nrepl)
;; (progn
;;   (clomacs-launch-nrepl nil t) ; sync nrepl launch.
;;   (clomacs-init))
;; (clomacs-init)
;; (clomacs-print-cp)

;; (add-to-list 'load-path "~/.emacs.d/clomacs/test/clomacs/")
;; (clomacs-load "load_test.clj")

;; (clomacs-defun hi-clojure clomacs.load-test/hi nil :stdout)
;; (hi-clojure)

(provide 'clomacs)

