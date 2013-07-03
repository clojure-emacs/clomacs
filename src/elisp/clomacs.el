;;; Commentary

;; `clomacs-ensure-nrepl-runnig' - Ensures nrepl is runnig. If not, launch it,
;; return nil. Return t otherwise.
;; `clomacs-load' Evaluate clojure side, run startup initialization
;; functions.
;; `clomacs-defun' - core clojure to elisp function wrapper.

(require 'nrepl)
(require 'clomacs-lib)

(defvar clomacs-nrepl-connrection-buffer-name (nrepl-connection-buffer-name))

(defvar clomacs-verify-nrepl-on-call t)

// TODO:
(defvar clomacs-autoload-nrepl-on-call t)

(defvar clomacs-clojure-offline-file "clomacs.clj"
  "Clojure-offline src helper file name.")

(defvar clomacs-is-initialized nil
  "When nil `clomacs-clojure-offline-file' is not loaded yet, t otherwise.")

(eval-and-compile
  (defvar clomacs-elisp-path
    (let ((path (or (locate-library "clomacs") load-file-name)))
      (and path (file-name-directory path)))
    "Directory containing the clomacs elisp code."))

(defun clomacs-is-nrepl-runnig ()
  "Return t if nrepl process is running, nil otherwise."
  (let ((ncb (get-buffer clomacs-nrepl-connrection-buffer-name)))
    (save-excursion
      (if (and clomacs-nrepl-connrection-buffer-name
               (buffer-live-p ncb)
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

(defmacro clomacs-defun (el-func-name
                         cl-func-name
                         &optional
                         return-type
                         return-value)
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
       (when  clomacs-verify-nrepl-on-call
         (if (not (clomacs-is-nrepl-runnig))
             (error
              (concat "Nrepl is not launched! You can launch it via "
                      "M-x clomacs-ensure-nrepl-runnig"))))
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
            (sleep-for 0.1)))))
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
          (clomacs-add-to-cp (file-name-directory (expand-file-name ".." clof))))
        (setq clomacs-is-initialized t))
    ;; nrepl is not running
    (setq clomacs-is-initialized nil))

(defun clomacs-load (clojure-side-file)
  "Evaluate clojure side."
  (clomacs-init)
  (let ((to-load (find-file-in-load-path clojure-side-file)))
    ;; add clojure-side files to classpath
    (clomacs-add-to-cp (file-name-directory (expand-file-name ".." to-load)))
    ;; load  clojure-side file
    (nrepl-load-file to-load)))

(defun clomacs-find-project-file (lib-name)
  (let ((path (file-name-directory (locate-library lib-name))))
    (find-file-upwards "project.clj" path 2)))

(defvar clomacs-project-file
  (clomacs-find-project-file "clomacs"))


;; (clomacs-is-nrepl-runnig)
;; (clomacs-ensure-nrepl-runnig)
;; (clomacs-launch-nrepl)
;; (clomacs-init)
;; (clomacs-print-cp)

;; (add-to-list 'load-path "~/.emacs.d/clomacs/test/clomacs/")
;; (clomacs-load "load_test.clj")

;; (clomacs-defun hi-clojure clomacs.load-test/hi nil :stdout)
;; (hi-clojure)

(provide 'clomacs)
