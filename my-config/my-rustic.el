;;; my-config/my-rustic.el -*- lexical-binding: t; -*-

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

(defun my-rustic-env-reset()
  "Reset all \"my-rustic-env-*\" variables."
  (interactive)
  (setq
   my-rustic-env-native-build '()
   my-rustic-env-target-build '()
   my-rustic-env-native-run '()))

(defun my-rustic-select-config(&optional cfg-name)
  "blaj"
  (interactive)
  (let* ((cfg-name (or cfg-name
                       (completing-read "Select Rust config: " (mapcar 'car my-rustic-configs))))
         (cfg (cdr (assoc-string cfg-name my-rustic-configs))))
    (message "%s" cfg)
    (when (plist-member cfg :env-native-build)
          (setq my-rustic-env-native-build (plist-get cfg :env-native-build)))
    (when (plist-member cfg :env-target-build)
          (setq my-rustic-env-target-build (plist-get cfg :env-target-build)))
    (when (plist-member cfg :env-native-run)
          (setq my-rustic-env-native-run (plist-get cfg :env-native-run)))))
