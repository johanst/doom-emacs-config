;;; my-config/my-rustic.el -*- lexical-binding: t; -*-
(require 'rustic)
(require 'my-utils)
(require 'apheleia)
(require 'tree-sitter)

(defvar my-rustic-env-native-build '()
  "alist of ENV_VAR_NAME/VALUE pairs that will be applied to process
environment when executing \"cargo run\" or \"cargo test\", i.e.
anything that always needs to run natively independently from
what target is currently chosen.

Note that this variable is intended for things needed in the
build process, e.g. \"PKG_CONFIG_PATH\"")

(defvar my-rustic-env-target-build '()
  "alist of ENV_VAR_NAME/VALUE pairs that will be applied to process
environment when executing any cargo command except for \"cargo
run\" or \"cargo test\", which requires always building natively
whatever the configuration of the target")

(defvar my-rustic-env-native-run '()
  "alist of ENV_VAR_NAME/VALUE pairs that will be applied to process
environment when executing \"cargo run\" or \"cargo test\", i.e.
anything that always needs to run natively independently from
what target is currently chosen.

Note that this variable is intended for things needed to run the
application or test, e.g. \"LD_LIBRARY_PATH\"")

(defvar my-rustic-configs '(("empty" . '()))
  "alist of \"name\" / plist values where 'name' is a descriptive
 name for the particular rust build configuration described by
  'plist'. The keywords in plist correspond to variables named
  my-rustic-*, i.e.
     :env-native-build value => set my-rustic-env-native-build to
  value This variable is expected to be set in a directory local
  variable within a project to describe the specific Rust
  build/run settings of that particular project."
 )

(defvar my-rustic-selected-config nil
  "Current selected config.")

(defvar my-rustic-compile-env nil
  "The environment that will be applied before running rustic-compilation-start,
i.e. what rustic-recompile will apply before running a compile with 'rustic-compile-command'.
Thus, all rustic build  commands that trigger rustic-compilation-start will need
to set this variable.")

(defun my-rustic-env-reset()
  "Reset all \"my-rustic-env-*\" variables."
  (interactive)
  (setq
   my-rustic-env-native-build '()
   my-rustic-env-target-build '()
   my-rustic-env-native-run '()))

(defun my-rustic-select-config(&optional cfg-name)
  "Select a config given by CFG-NAME.
Must be something from `my-rustic-configs'"
  (interactive)
  (let* ((cfg-name (or cfg-name
                       (completing-read "Select Rust config: " (mapcar 'car my-rustic-configs))))
         (cfg (cdr (assoc-string cfg-name my-rustic-configs))))
    (setq-default my-rustic-selected-config cfg-name)
    (when (plist-member cfg :env-native-build)
          (setq my-rustic-env-native-build (plist-get cfg :env-native-build)))
    (when (plist-member cfg :env-target-build)
          (setq my-rustic-env-target-build (plist-get cfg :env-target-build)))
    (when (plist-member cfg :env-native-run)
          (setq my-rustic-env-native-run (plist-get cfg :env-native-run)))))

(defun my-rustic-store-native-env()
  "Store the currently selected native build environment for rustic compiling."
  (setq my-rustic-compile-env
              (env-get-process-environment-from-alist my-rustic-env-native-build)))

(defun my-rustic-advice-store-native-env(func &rest args)
  "Adapter for calling `my-rustic-store-native-env' as :around advice.
The call is made before calling FUNC(ARGS)."
  (my-rustic-store-native-env)
  (apply func args))
;; advice cargo-run-command

(defun my-rustic-advice-activate-native-build-run-env(func &rest args)
  "Apply the currently selected rust native environment before running FUNC(ARGS).
The environment is combined from both build and run environemnt which makes
this advice suitable for applying before something is both built and run."
  (let* ((build-run-env-alist (append my-rustic-env-native-run my-rustic-env-native-build))
         (process-environment
          (env-get-process-environment-from-alist build-run-env-alist)))
    ;; (message "%s" process-environment)
    (apply func args)))
;; test/run

(defun my-rustic-store-target-env()
  "Store the currently selected target build environment for rustic compiling."
  (setq my-rustic-compile-env
        (env-get-process-environment-from-alist my-rustic-env-target-build)))

(defun my-rustic-advice-activate-stored-env(func &rest args)
  "Active the previously stored process environment.
Any function that starts a compile command that triggers
`rustic-compilation-start' should store its environment so it is
picked up by this function which sets the process environment
before running `rustic-compilation-start'. This also makes rustic-recompile work
correctly.
FUNC(ARGS) should be `rustic-compilation-start' called with ARGS."
  (let ((process-environment my-rustic-compile-env))
    (apply func args)))

(after! rustic
  ;; N.B. we cannot get this builtin convenience commands to build for any specific
  ;; target because we cannot just inject '--target=...' without destroying the
  ;; possibility to apply custom compile commands.
  ;; So therefore we just let these commands handle the native case.

  ;; cargo build, check, clippy, add, etc.
  (advice-add #'rustic-run-cargo-command :around #'my-rustic-advice-store-native-env)
  ;; recompile after rustic-run-cargo-command
  (advice-add #'rustic-compilation-start :around #'my-rustic-advice-activate-stored-env)
  ;;
  ;; cargo run +rerun
  (advice-add #'rustic-cargo-run-command :around #'my-rustic-advice-activate-native-build-run-env)
  ;;
  ;; cargo test <test-at-point>
  (advice-add #'rustic-cargo-current-test :around #'my-rustic-advice-activate-native-build-run-env)
  ;; cargo test +rerun
  (advice-add #'rustic-cargo-test-run :around #'my-rustic-advice-activate-native-build-run-env))

(defvar-local my-rustic-compile-commands-alist nil
  "An alist of compile-commands.
compile-commands is form (
  (\"cmd\" . (:config \"config\" :command \"cargo build....\"))...
)

where \"cmd\" is the identifier of this compile command,
`:config' is an optional plist entry that identifies the rustic config to use if
 any (must match an entry in `my-rustic-configs'
`:command' is the actual cargo command to run

`cmd' may also be a string, in which case it just represents the command without
any other options.")

(defun my-rustic-compile()
  "Select a cargo compile command and run it with `rustic-compile'.
Preferably command is set via dir-locals in project."
  (interactive)
  (if my-rustic-compile-commands-alist
    (if-let* ((key (completing-read "Select compile command: "
                                    (mapcar 'car my-rustic-compile-commands-alist)))
              (cmd (cdr (assoc key my-rustic-compile-commands-alist)))
              (command (if (listp cmd) (plist-get cmd :command) cmd)))
        (progn
          (when-let ((cfg-name (and (listp cmd) (plist-get cmd :config))))
            (my-rustic-select-config cfg-name)
            (my-rustic-store-target-env))
          (setq compilation-arguments nil)
          (setq rustic-compile-command command)
          (rustic-compile))
      (error "Invalid compile command"))
    (error "No commands in `my-rustic-compile-commands-alist'")))

(after! rustic
  ;; tree-sitter-mode will provide text objects like loop, function call, etc.
  (add-hook 'rustic-mode-hook #'tree-sitter-mode)
  ;; there can be only one rust formatter
  (add-hook 'rustic-mode-hook (lambda () (apheleia-mode t)))
  ;; and it shall use edition 2021 to handle async
  (add-to-list
   'apheleia-formatters
    '(rustfmt . ("rustfmt" "--edition" "2021" "--quiet" "--emit" "stdout")))
  )

(provide 'my-rustic)
;;; my-rustic.el ends here
