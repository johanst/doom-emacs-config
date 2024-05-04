;;; testing.el -*- lexical-binding: t; -*-
;;;
;;; -- Various things in progress --

(defun env-alist-from-key-value-file(file)
  "Given a file containing lines like KEY=value, produce a corresponding alist
like ((\"KEY1\" . \"value1\") (\"KEY2\" . \"value2\"))"
  (interactive "f.env file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((alist '()))
      (while (re-search-forward "^\\([^=\n]+\\)=\"?\\([^\"\n]+\\)\"?$" nil t)
        (push `(,(match-string 1) . ,(match-string 2)) alist))
      alist)))

(defun env-get-process-environment-from-alist(alist &optional include-keys)
  "Given an alist with KEY/VALUE elements, combine the existing
process-environment with the alist so that the alist values adds
or replaces lines like 'KEY=VALUE' that can later on be used to
set the process-environment.

'include-keys' is a list of strings that specifies which keys
from the alist that should be used. may be used to filter only
specific keys. nil means every key is accepted."
  (interactive "xgimme an alist: ")
  (let ((env (copy-sequence process-environment)))
    (dolist (item alist)
      (let ((key (car item))
            (value (cdr item)))
        (when (or (not include-keys) (member key include-keys))
          (progn
            (setq env
                  (cl-remove-if
                   (lambda(x) (string-prefix-p (concat key "=") x)) env))
            (push (concat key "=" value) env)))))
    env))

(defun env-plist-from-alist(alist &optional include-keys)
  "Given an alist with KEY/VALUE elements, produce a plist like
(:KEY1 \"value\" :KEY2 \"value2\"). It happens to be just what eglot and
dape wants for use in its jsonrpc requests.

'include-keys' is a list of strings that specifies which keys
from the alist that should be used. may be used to filter only
specific keys. nil means every key is accepted."
  (interactive "xgimme an alist: ")
  (let ((plist '()))
    (dolist (item alist)
      (let ((key (car item))
            (val (cdr item)))
        (when (or (not include-keys) (member key include-keys))
          (setq plist (append plist (list (intern (concat ":" key)) val))))))
    plist))


(dolist (item alist)
      (let ((key (car item))
            (val (cdr item)))
        (when t
          (setq knas )
          (setq gurka val))))))

(env-plist-from-alist
               '(("WSLENV" . "apa")
                 ("EDITOR" . "emacs")
                 ("TMUX_PANE" . "jupp")
                 )
               '("WSLENV" "EDITOR")
               )

(env-get-process-environment-from-alist
               '(("WSLENV" . "apa")
                 ("EDITOR" . "emacs")
                 ("TMUX_PANE" . "jupp")
                 )
               '("WSLENV" "EDITOR")
               )

(setq test-plist '(:key1 "v1" :key2 "v2"))
(setq mykey :key1)
(plist-get test-plist mykey)
(setq knas '("KWHCALC_BUILD_ENV" . "NATIVE"))

     (setq njatt
      '(("RUSTFLAGS" . "--target aarch64-unknown-linux-gnu"))
      )

      (setq-local
        njett
        `(:native
          ,(env-plist-from-alist njatt)
          )
        )
;; global johast-eglot-config
;; :rust-build-env alist/env-file ( native or rustic build +  )
;;    :build-env for eglot (architecture, path to cargo etc.)
;; :rust-run-env (native rustic test + run )
;; :debug-env (native or cross)
;;   rustic-cargo-bin
;;   rustic-cargo-build-exec-command
;;   rustic-cargo-build-arguments
;;
;; rustic run-command (that always need native run + build env):
;;   rustic-cargo-test-run
;;   rustic-cargo-run-test
;;   rustic-cargo-run
;; rustic build-command (that only builds so should use native|target buildenv)
;; all ends in rustic-compilation-start...
;;
;; apparently we need inheritenv from envrc
(setq my-list (cl-remove-if (lambda (x) (and (stringp x) (string-prefix-p prefix-to-remove x))) my-list))
(message "%s" (env-get-process-environment-from-alist '((2 . 3))))
(message "%s" (env-alist-from-key-value-file "key_value.txt"))

(defun plist-from-key-value-file (file)
  "Construct a plist from a key-value file."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((plist '()))
      (while (re-search-forward "\\([^=]+\\)=\\(.+\\)$" nil t)
        (setq plist (cons (intern (match-string 1)) (cons (match-string 2) plist)))))
      (apply 'plist-put plist)))
;; Example usage:
(setq my-plist (plist-from-key-value-file "key_value.txt"))

`(,(intern (concat ":" "hej")) "podaj")

(defvar-keymap johast-moving-repeat-map
  :doc "Keymap to repeat `next-buffer' and `previous-buffer'.  Used in `repeat-mode'."
  :repeat t
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-list
  "p" #'backward-list
  "u" #'backward-up-list
  "d"  #'down-list)

;; johast-build-environment
;; johast-run-environment

(setq alist '())
(add-to-list 'alist '(2 . 3))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-unfold-all))
  (if (+fold--ts-fold-p)
      (ts-fold-open-all)
    (save-excursion
      (+fold--ensure-hideshow-mode)
      (if (integerp level)
          (progn
            (outline-hide-sublevels (max 1 (1- level)))
            (hs-life-goes-on
             (hs-hide-level-recursive (1- level) (point-min) (point-max))))
        (hs-show-all)
        (when (fboundp 'outline-show-all)
          (outline-show-all))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-refold-all))
  (save-excursion
    (if (+fold--ts-fold-p)
        (ts-fold-close-all)
      (progn
        (+fold--ensure-hideshow-mode)
        (hs-life-goes-on
         (if (integerp level)
             (progn
               (outline--show-headings-up-to-level (1+ level))
               (hs-hide-level-recursive (1- level) (point-min) (point-max)))
           (hs-hide-all)
           (when (fboundp 'outline-hide-sublevels)
             (outline-show-only-headings))))))))


;; https://www.reddit.com/r/emacs/comments/12g30nz/keybindings_for_info_when_using_evil/
(defun ev/evil-collection-visit-file (&optional mode)
    "Visit the `evil-collection' source file corresponding to
  the current major mode, or to MODE if specified, in a new
  window."
    (interactive)
    (let* ((mode (or mode (format "%s" major-mode)))
           (base-path (concat (file-name-directory (find-library-name "evil-collection")) "modes/"))
           (short (replace-regexp-in-string "-.+$" "" mode))
           (file-list (directory-files-recursively base-path (format ".*%s.*\\.el$" short)))
           (choice (cond ((= (length file-list) 1)
                          (car file-list))
                         ((> (length file-list) 1)
                          (completing-read "Multiple evil-collection files found. Visit: " file-list nil t))
                         (t
                          (user-error "No evil-collection file found for %s" (format "%s" mode))))))

        (select-window (display-buffer (find-file-noselect choice)
                                       '(display-buffer-at-bottom . ())))
      (let ((scroll-preserve-screen-position t)
            (cur-line (line-number-at-pos))
            (start (line-number-at-pos (window-start))))

        (beginning-of-buffer)
        (re-search-forward "(evil-collection-define-key")
        (beginning-of-line)
        (scroll-up (- cur-line start)))))

(defun my-dape-process-output-buffer-name()
  "hej"
  (concat "*dape process output"
         (if (project-current) (concat " <" (project-name (project-current)) ">") "") "*"))

(defun my-dape-get-process-output-buffer()
  "hopp"
  (with-current-buffer
      (get-buffer-create (my-dape-process-output-buffer-name))
    ;; stolen without shame from compile.el.gz
    (let ((comp-proc (get-buffer-process (current-buffer))))
      (when comp-proc
        (condition-case ()
            (progn
              (interrupt-process comp-proc)
              (sit-for 1)
              (delete-process comp-proc))
          (error nil))))
    (current-buffer)))

(defun my-open-logfile-terminal ()
  "Open a read-only terminal in Emacs showing the output of a logfile using tail -f."
  (interactive)
  (let* ((buffer (my-dape-get-process-output-buffer))
         (win))
    (with-current-buffer buffer
      (comint-mode)
      (insert "some text has to be inserted to make the window display something\n")
      ;; (evil-force-normal-state)
      (setq win (display-buffer buffer '(display-buffer-use-some-window (inhibit-same-window . t))))
      ;; enable scroll
      (setq-local window-point-insertion-type t)
      ;; (set-window-start win (point-max))
      ;; (setq-local evil-default-state 'normal)
      ;; (goto-char (point-max))
      (goto-char (point-max))
      ;; (start-process-shell-command "knas" (current-buffer) "/home/johast/hacking/slask/number.sh\n")
      (start-process-shell-command
       "knas"
       (current-buffer)
       "ssh johan@192.168.2.141 gdbserver :1234 /home/johan/.local/bin/kwhcalc_debug\n")
      ;; (sit-for 1)
      ;; (run-at-time 0.1 nil
      ;;              (lambda ()
      ;;                (goto-char (point-max))
      ;;                (redisplay t)))
      ;; (redisplay t)
      )))



(generate-new-buffer-name "hej")
(display-buffer-in-side-window )
(display-buffer-in-side-window (generate-new-buffer "hej") '((side . right)))

;; gdb-mi

(setq gdb-many-windows t)
(setq gdb-restore-window-configuration-after-quit t)
(setq gdb-debuginfod-enable-setting t)

(defvar hack-gdb-mi t)

(defun my-gdb-inferior-io-mode-hook ()
  "Hook func"
  (when hack-gdb-mi
      (start-process-shell-command
       "knas"
       (current-buffer)
       "gdbserver :1234 /home/johast/hacking/cpptest/build/dbgtest\n")))

(add-hook 'gdb-inferior-io-mode-hook #'my-gdb-inferior-io-mode-hook)
(remove-hook 'gdb-inferior-io-mode-hook #'my-gdb-inferior-io-mode-hook)

(defun my-gdb-inferior-io--init-proc-advice(func &rest args)
  "Adapter for maybe calling `my-gdb-inferior-io--init-proc-advice' as :around advice."
  (when (not hack-gdb-mi)
    (apply func args)))

(advice-add #'gdb-inferior-io--init-proc :around #'my-gdb-inferior-io--init-proc-advice)

(gdb "gdb -i=mi ~/hacking/cpptest/build/dbgtest -ex \"target remote localhost:1234\"")

(defun my-gdb-restart()
  "Restart current gdb remote session"
  (interactive)
  ;; interrupt gdb if running
  (when gud-running
    (with-current-buffer gud-comint-buffer
      (comint-interrupt-subjob)))
  ;; make sure debugged program terminates
  (gdb-io-quit)
  ;; detach from existing process
  (gud-basic-call "detach")
  ;; retrigger hook to start gdbserver
  (with-current-buffer (gdb-get-buffer 'gdb-inferior-io)
    (gdb-inferior-io-mode))
  ;; reattach from existing process
  (gud-basic-call "target remote localhost:1234"))

;; interrupt gdb if running
(when gdb-running
  (with-current-buffer gud-comint-buffer
    (comint-interrupt-subjob)))

;; make sure debugged program is quit
(when-let* ((buf (gdb-get-buffer 'gdb-inferior-io))
            (proc (get-buffer-process buf)))
  (gdb-io-quit))

;; detach from existing process
(gud-basic-call "detach")

;; retrigger hook to start gdbserver
(with-current-buffer (gdb-get-buffer 'gdb-inferior-io)
  (gdb-inferior-io-mode))

;; reattach from existing process
(gud-basic-call "target remote localhost:1234")

  (message "hej %s" (process-status proc)))

(gud-basic-call "detach")
(defun test-get-proc ()
  (interactive)
(when-let* ((buf (gdb-get-buffer 'gdb-inferior-io))
            (proc (get-buffer-process buf)))
  (message "hej %s" (process-status proc))
))

(defun my-gdb-exit-hook()
  (message "exiting gdb"))

(advice-add 'gdb-reset :after 'my-gdb-exit-hook)

;; gud-comint-buffer))
