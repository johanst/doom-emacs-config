;;; my-config/my-gdb.el -*- lexical-binding: t; -*-
(require 'gdb-mi)
(require 'my-utils)

(setq gdb-many-windows t)
(setq gdb-restore-window-configuration-after-quit t)
(setq gdb-debuginfod-enable-setting t)

(defvar my-gdbserver-command nil
  "Command that will be used to start gdbserver in inferior window,
e.g. \"gdbserver 192.168.2.141:1234 ~/myprog\"")

(defvar my-gdbserver-env '()
  "alist of KEY/VALUE environment variables to be set before starting gdbserver")

(defvar my-gdb-command nil
  "Command to start gdb that must contain -i=mi and should contain option to
attach to gdbserver if `my-gdbserver-command' is set, e.g.
`gdb -i=mi ~/myprog -ex \"target remote 192.168.2.141:1234\"'")

(defvar my-gdb-env '()
  "alist of KEY/VALUE environment variables to be set before starting gdb")

(defun my-gdb-config-reset()
  "Reset all my-gdb settings"
  (interactive)
  (setq my-gdbserver-command nil
        my-gdbserver-env nil
        my-gdb-command nil
        my-gdb-env nil))

(defun my-gdb-inferior-io-mode-hook ()
  "Hack to intercept gdb-inferior-io process to install our launch of gdbserver
if `my-gdbserver-command' is set."
  (when my-gdbserver-command
    (let ((process-environment (env-get-process-environment-from-alist my-gdbserver-env)))
      (start-process-shell-command
       "gdbserver"
       (current-buffer)
       (concat my-gdbserver-command "\n")))))

;; Hack to intercept gdb-inferior-io process to install our launch of gdbserver
;; if `my-gdbserver-command' is set."
(add-hook 'gdb-inferior-io-mode-hook #'my-gdb-inferior-io-mode-hook)

(defun my-gdb-inferior-io--init-proc-advice(func &rest args)
  "Hack to disable regular gdb-mi code for setting up the inferior when we have
done it ourselves using `my-gdbserver-command'."
  (when (not my-gdbserver-command)
    (apply func args)))

;; Hack to disable regular gdb-mi code for setting up the inferior when we have
;; done it ourselves using `my-gdbserver-command'."
(advice-add #'gdb-inferior-io--init-proc :around #'my-gdb-inferior-io--init-proc-advice)

(defun my-gdb-pause()
  "Pause debugged program. Apparently a missing command in gdb/gud"
  (interactive)
  (when gud-running
    (with-current-buffer gud-comint-buffer
      (comint-interrupt-subjob))))

(defun my-gdb-start()
  "Start gdb with given configuration in my-gdb-* variables"
  (interactive)
  (setq my-last-debug-command #'my-gdb-start)
  (global-set-key (kbd "<f9>") #'gud-break)
  (global-set-key (kbd "S-<f9>") #'gud-clear)
  (global-set-key (kbd "C-<f9>") #'gud-tbreak)
  (global-set-key (kbd "<f10>") #'gud-next)
  (global-set-key (kbd "C-<f10>") #'gud-until)
  (global-set-key (kbd "<f11>") #'gud-step)
  (global-set-key (kbd "C-<f11>") #'gud-stepi)
  (global-set-key (kbd "S-<f11>") #'gud-finish)
  (global-set-key (kbd "<f12>") #'gud-cont)
  (global-set-key (kbd "C-<f12>") #'gud-run)
  (global-set-key (kbd "S-<f12>") #'my-gdb-pause)
  (global-set-key gud-key-prefix gud-global-map)
  (let ((process-environment (env-get-process-environment-from-alist my-gdb-env)))
    (gdb my-gdb-command)))

(defvar my-gdb-configs '(("empty" . '()))
  "alist of \"name\" / plist values where `name' is a descriptive
 name for the particular gdb configuration described by
  `plist'. The keywords in plist correspond to variables named
  my-gdb-*, i.e.
     :gdbserver-command value => set my-gdbserver-command to
  value. This variable is expected to be set in a directory local
  variable within a project to describe the specific gdb config
  for that particular project."
 )

(defvar my-gdb-command-history nil
  "History for selection of gdb commands.")

(defvar my-gdbserver-command-history nil
  "History for selection of gdbserver commands.")

(defun my-gdb-select-config-and-start(&optional cfg-name)
  "Select a gdb-config, preferably set via dir-locals in project, and then
run `my-gdb-start'."
  (interactive)
  (let* ((cfg-name (or cfg-name
                       (completing-read "Select gdb config: " (mapcar 'car my-gdb-configs))))
         (cfg (cdr (assoc-string cfg-name my-gdb-configs)))
         (gdbserver-command-default
           (and (plist-member cfg :gdbserver-command) (plist-get cfg :gdbserver-command)))
         (gdb-command-default
           (and (plist-member cfg :gdb-command) (plist-get cfg :gdb-command)))
         (gdbserver-command
          (read-from-minibuffer "gdbserver command: " gdbserver-command-default nil nil
                                'my-gdbserver-command-history ))
         (gdb-command
          (read-from-minibuffer "gdb command: " gdb-command-default nil nil
                                'my-gdb-command-history))
          )
    (setq my-gdbserver-command gdbserver-command
          my-gdbserver-env
          (and (plist-member cfg :gdbserver-env) (plist-get cfg :gdbserver-env))
          my-gdb-command gdb-command
          my-gdb-env
          (and (plist-member cfg :gdb-env) (plist-get cfg :gdb-env)))
    (my-gdb-start)))

(provide 'my-gdb)
