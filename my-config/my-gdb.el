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
\"gdb -i=mi ~/myprog -ex 'target remote 192.168.2.141:1234'\"")

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

(defun my-gdb-start()
  "Start gdb with given configuration in my-gdb-* variables"
  (interactive)
    (let ((process-environment (env-get-process-environment-from-alist my-gdb-env)))
      (gdb my-gdb-command)))

(provide 'my-gdb)
