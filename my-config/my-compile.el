;;; my-config/my-compile.el -*- lexical-binding: t; -*-
(require 'my-utils)

(defvar-local my-compile-commands-alist nil
  "An alist of compile-commands.
compile-commands is form (
  (\"cmd\" . (:env alist :command \"ninja build....\"))...
)

where \"cmd\" is the identifier of this compile command,
`:env' is an optional alist of KEY/VALUE pairs that will be added to
`compilation-environment' when running the compile command.
`:command' is the actual cargo command to run

`cmd' may also be a string, in which case it just represents the command without
any other options.")

(defun my-project-compile()
  "Select a compile command, preferably set via dir-locals in project and then
run `project-compile' with that command selected."
  (interactive)
  (if my-compile-commands-alist
    (if-let* ((key (completing-read "Select compile command: "
                                    (mapcar 'car my-compile-commands-alist)))
              (cmd (cdr (assoc key my-compile-commands-alist)))
              (command (if (listp cmd) (plist-get cmd :command) cmd)))
        (progn
          (when-let ((env (and (listp cmd) (plist-get cmd :env))))
            (setq compilation-environment
                  (env-key-value-list-from-alist env)))
          (setq compile-command command)
          (setq compilation-arguments nil)
          (project-compile))
      (error "Invalid compile command"))
    (error "No commands in `my-compile-commands-alist'")))

(provide 'my-compile)
