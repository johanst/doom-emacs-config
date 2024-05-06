;;; my-config/my-dape.el -*- lexical-binding: t; -*-

;; TODO: hooks for running/stopping gdbserver and recompile
(defvar-local my-dape-configs-alist nil
  "An alist of dape-commands, key is a string for selection, value is a plist
of form:
':config' actual config that can be passed to dape
':build-env' (optional) alist of environment variables needed on build machine
to actually run gdb (runtime environment is part of the config,
at least for gdb)")

(defvar my-dape-build-env-alist nil
  "Current selected build-environment for dape used by `my-dape-run'.
See `my-dape-configs-alist'.")

(defvar my-dape-config nil
  "Current selected dape config used by `my-dape-run'.
See `my-dape-configs-alist'.")

(defun my-dape-config-select-and-run-dape()
  "Select a dape-config, preferably set via dir-locals in project, and then
run `my-dape-run'."
  (interactive)
  (if my-dape-configs-alist
    (let* ((key (completing-read "Select dape command: " my-dape-configs-alist))
           (cfg (cdr (assoc key my-dape-configs-alist)))
           (build-env (and
                       (plist-member cfg :build-env)
                       (plist-get cfg :build-env)))
           (cmd (plist-get cfg :config)))
      (setq
       my-dape-build-env-alist build-env
       my-dape-config cmd)
      (my-dape-run))
    (message "No dape config available")))

(defun my-dape-run()
  "Run dape with environment and config set by
`my-dape-config-select-and-run-dape'."
  (interactive)
  (setq my-last-debug-command #'my-dape-run)
  (global-set-key (kbd "<f9>") #'dape-breakpoint-toggle)
  (global-set-key (kbd "<f10>") #'dape-next)
  (global-set-key (kbd "<f11>") #'dape-step-in)
  (global-set-key (kbd "S-<f11>") #'dape-step-out)
  (global-set-key (kbd "<f12>") #'dape-continue)
  (global-set-key (kbd "C-<f12>") #'dape-restart)
  (global-set-key (kbd "S-<f12>") #'dape-pause)
  (when dape-key-prefix (global-set-key dape-key-prefix dape-global-map))
  (let ((process-environment
         (env-get-process-environment-from-alist my-dape-build-env-alist)))
    (dape my-dape-config)))

(provide 'my-dape)
