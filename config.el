;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-load-path! "my-config")
(require 'my-utils)

(require 'inheritenv)
(require 'my-rustic)

(defvar my-last-debug-command nil
  "Last command that started a debug session.
Facilitates having a single keybinding for restarting the debug session
whatever debugger was used")
(require 'my-gdb)
(require 'my-dape)

(require 'my-compile)

(setq doom-leader-alt-key "M-j")
(setq doom-localleader-alt-key "M-j m")

(setq git-commit-summary-max-length 60)

(repeat-mode 1)

(after! cc-mode
  ;; tree-sitter-mode will provide text objects like loop, function call, etc.
  (add-hook 'c-mode-common-hook #'tree-sitter-mode))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(after! evil-easymotion
  (setq avy-all-windows t))

(after! apheleia
  ;; Use shfmt as default formatter for sh-mode as well (apheleia only defines bash-ts-mode)
  (setq apheleia-mode-alist (cons '(sh-mode . shfmt) apheleia-mode-alist)))

(after! project
  (setq project-vc-extra-root-markers '(".dir-locals.el")))

(require 'my-org)

;; Configure LSP servers
;; disable inlay hints by default (keybinding for toggling it)
;; clangd
;;   always allow a `.clangd` config file (why is this not default?)
(after! eglot
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) .
                 ("clangd" "--enable-config"))))

(defun johast-treeemacs-toggle()
  "If we're in main workspace just do it the doom way, i.e. add projects/perspectives
as we go. But for other workspaces we don't want to pollute the setup and there we require
any project to be explicitly added to treemacs."
  (interactive)
  (if (string= "main" (+workspace-current-name))
      (+treemacs/toggle)
    (treemacs)))

(defvar-local johast-compile-commands-alist nil
  "An alist of compile-commands, key is for selection, value is the actual command")

(defun johast-project-compile()
  "Select a compile command, preferably set via dir-locals in project and then
run `project-compile' with that command selected."
  (interactive)
  (when johast-compile-commands-alist
    (let* ((key (completing-read "Select compile command: " johast-compile-commands-alist))
           (cmd (cdr (assoc key johast-compile-commands-alist))))
      (setq compile-command cmd))
    (project-compile)))


(defun my-debug-run ()
  "Relaunch last debug session"
  (interactive)
  (if my-last-debug-command
      (funcall my-last-debug-command)
    (error "No previous gdb session")))

;; That view-mode is even better than evil normal mode when we just want to
;; read something. So let's make it convenient.

(defvar johast-evil-mode-previous-state nil
  "A variable that holds the evil-mode state that was used prior to entering
 view-mode.")

(defun johast-view-mode-save-restore-evil-state()
  (if view-mode
      (progn
        (setq-local johast-evil-mode-previous-state evil-state)
        (evil-emacs-state))
    (when johast-evil-mode-previous-state
    (setq-local evil-state johast-evil-mode-previous-state))))

(after! view
  (add-hook 'view-mode-hook #'johast-view-mode-save-restore-evil-state))

(map!
 :leader
 (:prefix "c"
  :desc "Toggle inlay hints" :n "h" #'eglot-inlay-hints-mode)
 (:prefix-map ("j". "johast")
  :desc "Project compile" :n "C" #'my-project-compile
  :desc "Project recompile" :n "c" #'project-recompile
  (:prefix "D"
   :desc "Select & run dape config" :n "A" #'my-dape-config-select-and-run-dape
   :desc "Select & run gdb config" :n "D" #'my-gdb-select-config-and-start)
  :desc "Relaunch debug session" :n "d" #'my-debug-run
  :desc "Org topic dired" :n "t" #'my-org-topics-dired
  :desc "Org topic sync" :n "T" #'my-org-topics-sync
  :desc "Treemacs focus" :n "p" #'treemacs-select-window
  :desc "View mode" :n "v" #'view-mode)
 (:prefix "o"
  :desc "Treemacs toggle" :n "p" #'johast-treeemacs-toggle))

;; Alternative to Ctrl-I/Ctrl-O in terminal mode
;; (Ctrl-I is indistinguashible from TAB)
(global-set-key (kbd "<f3>") #'better-jumper-jump-backward)
(global-set-key (kbd "<f4>") #'better-jumper-jump-forward)
;; let's add workspaces for now (beware of M-f4 on Windows though)
(global-set-key (kbd "<M-f3>") #'+workspace/cycle)

;; Alternative to C-; and C-c C-; in terminal mode
(map!
 "M-," #'embark-act
 (:map minibuffer-local-map
       "M-,"               #'embark-act
       "C-c M-,"           #'embark-export
       "C-c C-p"           #'+pass/consult))

(map!
 :after cc-mode
 :map (c-mode-map c++-mode-map)
 (:prefix "C-c"
  :desc "Find other file" "o" #'projectile-find-other-file))

(map!
 :after view
 :map view-mode-map
 ;; j/k are unbound in view-mode so we can grab them for standard evil behaviour
 "j" #'next-line
 "k" #'previous-line
 ;; RET will sometimes not only scroll line forward. Occasionally it also jumps away
 ;; when there is something at point. "t" is close to "y" so why not...
 "t" #'View-scroll-line-forward)

;; C-x <up>/<down> seems to be unassigned so we can use it as a general
;; back/forward within a specific mode (like info).
;; Apparently doom, evil or something has messed up info-mode so that
;; ctrl-i/ctrl-o is not symmetrical, and again ctrl-i is not terminal friendly
;; anyway
(map!
 :after info
 :map Info-mode-map
 "C-x <up>" #'Info-history-back
 "C-x <down>" #'Info-history-forward)

(defvar-keymap johast-info-repeat-map
  :doc "Keymap to repeat `Info-history-back' and `Info-history-forward'.
Used in `repeat-mode'."
  :repeat t
  "<up>" #'Info-history-back
  "<down>" #'Info-history-forward)
