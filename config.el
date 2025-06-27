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
(setq doom-theme 'doom-gruvbox-light)

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

(let ((dir (expand-file-name "my-confidential" doom-user-dir)))
  (when (and (file-exists-p dir) (file-directory-p dir))
    (add-load-path! "my-confidential")
    (require 'my-confidential)))

(setq doom-leader-alt-key "M-j")
(setq doom-localleader-alt-key "M-j m")

(setq git-commit-summary-max-length 60)

(repeat-mode 1)

(defun my/show-trailing-whitespace-in-text-modes ()
  "Enable trailing whitespace visual markers in text, programming, and org modes."
  (when (derived-mode-p 'text-mode 'prog-mode 'org-mode)  ; Text, programming, and org modes
    (setq show-trailing-whitespace t)))

(add-hook 'after-change-major-mode-hook 'my/show-trailing-whitespace-in-text-modes)

(after! cc-mode
  ;; tree-sitter-mode will provide text objects like loop, function call, etc.
  (add-hook 'c-mode-common-hook #'tree-sitter-mode))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(after! evil-easymotion
  (setq avy-all-windows t))

;; Just like in modules/ui/window-select.el, but maps to M-o and does not mess with other-window
(use-package! ace-window
  :unless (modulep! +switch-window)
  :defer t
  :init
  (global-set-key (kbd "M-o") #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame
        aw-background t))

(after! apheleia
  ;; Use shfmt as default formatter for sh-mode as well (apheleia only defines bash-ts-mode)
  (setq apheleia-mode-alist
        (append '((sh-mode . shfmt)
                  (nim-mode . nimpretty))
                apheleia-mode-alist)))

(after! diff-hl
  ;; FIXME: The bug described below makes vc-gutter unusable. Tried a while without async
  ;; and the result is that emacs gets gradually more and more unresponsive until it
  ;; has to be shut down. For now, disable vc-gutter.
  ;;
  ;; There seems to be a bug in doom's fix for killing threads, see
  ;; ~/.config/emacs/modules/ui/vc-gutter/config.el:193
  ;; Seems there is a race sometimes causing the lambda performing the thread killing to go wild
  ;; and use lots of memory. Let's try without async for a while.
  (setq diff-hl-update-async nil))

(after! nim-mode
  (add-hook 'nim-mode-hook (lambda () (apheleia-mode t)))
  (add-hook 'nim-mode-local-vars-hook #'lsp! 'append)
  ;; Make sure apheleia can locate nimpretty. On WSL installation I had to make
  ;; a symbolic link from /usr/bin/nimpretty to the actual location in $HOME
  (add-to-list
   'apheleia-formatters
    '(nimpretty . ("nimpretty" "--indent:2" inplace)))
  )

(after! project
  (setq project-vc-extra-root-markers '(".dir-locals.el")))

(require 'my-org)

;; Configure LSP servers
;; disable inlay hints by default (keybinding for toggling it)
;; clangd
;;   always allow a `.clangd` config file (why is this not default?)
;;   also add "--query-driver=/**/*" to make sure cross compiles work by forcing
;;   clangd to properly pick up the cross compiler sysroot from the actual compiler.
;;   See https://www.reddit.com/r/emacs/comments/1ascb93/configuring_clangd_on_a_project_basis_is_it/
(after! eglot
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) .
                 ("clangd"
                  "--enable-config"
                  "--query-driver=/**/*"
                  )))
  (add-to-list 'eglot-server-programs
               '((nim-mode) . ("nimlsp")))
  )

;; Add the missing git commands to magit
(after! magit
  (transient-append-suffix 'magit-fetch "-F"
    '("-U" "Unshallow" "--unshallow"))
  (transient-append-suffix 'magit-pull "-F"
    '("-U" "Unshallow" "--unshallow")))

(after! gptel
  (setq
   gptel-default-mode 'org-mode
   gptel-model 'gpt-4.1
   gptel-backend (gptel-make-gh-copilot "Copilot")))

(defun my-gptel-quick-with-context ()
  (interactive)
  (let ((old-context gptel-quick-use-context))
    (setq gptel-quick-use-context t)
    (save-excursion
      (call-interactively #'gptel-quick))
    (setq gptel-quick-use-context old-context)))

(after! gptel-quick
  (setq
   gptel-quick-timeout 120 ;; 2 minutes, but why would i ever want a timeout ?
   gptel-quick-word-count 50 ;; I would rarely want less than 50 words in an explanation
   ))

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
 :desc "Quick goto anywhere" :n "U" #'evil-avy-goto-char-2
 :desc "Query replace" :vn "&" #'query-replace          ;; A whole lot easier than S-M-5 (%)
 :desc "Query replace" :vn "g&" #'query-replace-regexp  ;; not to mention C-S-M-5 (%)
 :leader
 (:prefix "c"
  :desc "Toggle inlay hints" :n "h" #'eglot-inlay-hints-mode)
 (:prefix-map ("j". "johast")
  :desc "Project compile" :n "C" #'my-project-compile
  :desc "Project recompile" :n "c" #'project-recompile
  :desc "Rustic compile" :n "R" #'my-rustic-compile
  :desc "Rustic recompile" :n "r" #'rustic-recompile
  (:prefix "D"
   :desc "Select & run dape config" :n "A" #'my-dape-config-select-and-run-dape
   :desc "Select & run gdb config" :n "D" #'my-gdb-select-config-and-start)
  (:prefix "d"
   :desc "Relaunch debug session" :n "d" #'my-debug-run
   :desc "Save breakpoints" :n "B" #'my-gdb-save-breakpoints
   :desc "Load breakpoints" :n "b" #'my-gdb-load-breakpoints
   )
  :desc "Org topic dired" :n "t" #'my-org-topics-dired
  :desc "Org topic sync" :n "T" #'my-org-topics-sync
  :desc "Treemacs focus" :n "p" #'treemacs-select-window
  :desc "View mode" :n "v" #'view-mode)
 )

(map!
 :desc "Relaunch debug session" "C-c d d" #'my-debug-run
 :desc "Save breakpoints" "C-c d B" #'my-gdb-save-breakpoints
 :desc "Load breakpoints" "C-c d b" #'my-gdb-load-breakpoints
 :desc "Select & run project dape config" "C-c D A" #'my-dape-config-select-and-run-dape
 :desc "Select & run project gdb config" "C-c D J" #'my-gdb-select-config-and-start
 ;; "C-c D D" is left for site-specific stuff in my-confidential
 :desc "Recompile" "C-c c" #'recompile
 ;; "C-c C C" is left for site-specific stuff in my-confidential
 :desc "Select & run project compile task" "C-c C J" #'my-project-compile
 :desc "gptel" "C-c l" #'gptel ;; l is for LLM...
 :desc "gptel" "C-c L" #'gptel-menu ;; l is for LLM...
 :desc "gptel" "C-c e" #'gptel-quick ;; l is for LLM...
 :desc "calc" "C-c *" #'calc ;; Slightly faster than C-x * c
 )

;; The emacs default comment-dwim" keybinding "M-;" does not work through certain terminals (nor does "C-;")
;; "C-/" is by default undo in emacs, but in evil-mode we have "u" for that so we can use "C-/" for
;; handling comments instead.
;; Furthermore WezTerm (and probably some other terminals) transmit C-/ as C-_ (basically ignoring
;; bit 6 and 7 of ASCII) so therefore we bind everything that is bound to C-/ to C-_ as well.
;; Because "comment-dwim" does not do what i mean when no region is marked (it adds a comment add the end of
;; the line instead of just commenting/uncommenting the whole line) we instead use "comment-line" when in
;; normal or insert mode and only "comment-dwim" when in visual mode (i.e. a region is marked).
(map!
 :desc "Comment line" :ni "C-/" #'comment-line
 :desc "Comment region" :v "C-/" #'comment-dwim
 :desc "Comment line" :ni "C-_" #'comment-line
 :desc "Comment region" :v "C-_" #'comment-dwim)

;; Alternative to Ctrl-I/Ctrl-O in terminal mode
;; (Ctrl-I is indistinguashible from TAB)
;; C-x <up>/<down> seems to be unassigned so we can use it as a general
;; back/forward
(global-set-key (kbd "C-x <up>") #'better-jumper-jump-backward)
(global-set-key (kbd "C-x <down>") #'better-jumper-jump-forward)

(defvar-keymap johast-better-jumper-repeat-map
  :doc "Keymap to repeat `better-jumper-*'.
Used in `repeat-mode'."
  :repeat t
  "<up>" #'better-jumper-jump-backward
  "<down>" #'better-jumper-jump-forward)

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

;; Specialized jumper for info mode...
;; Apparently doom, evil or something has messed up info-mode so that
;; ctrl-i/ctrl-o is not symmetrical, and again ctrl-i is not terminal friendly
;; anyway
(map!
 :after info
 :map Info-mode-map
 :n "TAB"     #'Info-next-reference
 "C-x <up>" #'Info-history-back
 "C-x <down>" #'Info-history-forward)

(defvar-keymap johast-info-repeat-map
  :doc "Keymap to repeat `Info-history-back' and `Info-history-forward'.
Used in `repeat-mode'."
  :repeat t
  "<up>" #'Info-history-back
  "<down>" #'Info-history-forward)
