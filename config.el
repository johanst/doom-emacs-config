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

(repeat-mode 1)

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(after! evil-easymotion
  (setq avy-all-windows t))

(after! org
  (setq org-ellipsis " ▼"))

(after! project
  (setq project-vc-extra-root-markers '(".dir-locals.el")))

(defvar johast-org-topic-dir
  (expand-file-name "topic" org-directory)
  "Directory for recording topic specific notes/todos/journal")

(defun johast-org-topics-list ()
  "Return a list of topics handled by org"
  (if (file-exists-p johast-org-topic-dir)
      (cl-remove-if (lambda (x) (member x '("." "..")))
                    (directory-files johast-org-topic-dir))
    nil))

(defun johast-org-topic-filename-select (filename)
  "Query user for a topic in org topic directory and return full path to
   filename within that directory"
  (let ((topic-filename
         (concat (file-name-as-directory johast-org-topic-dir)
                 (file-name-as-directory (completing-read "Select topic: " (johast-org-topics-list)))
                 filename)))
    topic-filename))

(defun johast-org-topic-filename-select-todo ()
  "Select org topic and return full path to its todo file"
  (johast-org-topic-filename-select "todo.org"))

(defun johast-org-topic-filename-select-notes ()
  "Select org topic and return full path to its notes file"
  (johast-org-topic-filename-select "notes.org"))

(defun johast-org-topic-filename-select-journal ()
  "Select org topic and return full path to its journal file"
  (johast-org-topic-filename-select "journal.org"))

(defun johast-org-topics-dired ()
  "Open dired in org-directory/topic for quick add/remove of directories that
   will correspond to topics "
  (interactive)
  (dired johast-org-topic-dir))

(defun johast-topicdirp (dirname)
  "Return t if parent directory is topic"
  (string-equal
   "topic"
   (file-name-base
    (directory-file-name (file-name-directory (expand-file-name dirname)))
    )))

(defun johast-org-topics-sync ()
  "Update org-agenda-files to match contents of org-directory/topic"
  (interactive)
  (setq
   org-agenda-files
   (cons org-directory
         (when (file-exists-p johast-org-topic-dir)
           (directory-files-recursively
            johast-org-topic-dir
            "\\.org$"
            t
            #'johast-topicdirp))))
  (message "Synchronized topic dir '%s'" johast-org-topic-dir))

(after! org
  ;; Org-cache seems to be messed up when treemacs is parsing the
  ;; projects and workspaces
  (setq org-element-use-cache nil)

  (johast-org-topics-sync)
  ;; Based on doom default but with option to use centralized topic specific org
  ;; files instead of project specific, which requires an asssociation with a
  ;; git repo or some other projectile-way of identifying a project.
  (setq
   org-capture-templates
   '(("t" "Todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("T" "Todo +clipboard" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* TODO %?\n%x" :prepend t)
     ("n" "Notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("N" "Notes +clipboard" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%x" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)

     ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
     ;; {todo,notes,changelog}.org file is found in a parent directory.
     ;; Uses the basename from `+org-capture-todo-file',
     ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
     ("p" "Templates for projects")
     ("pt" "Project-local todo" entry  ; {project-root}/todo.org
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("pT" "Project-local todo +clipboard" entry  ; {project-root}/todo.org
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%x" :prepend t)
     ("pn" "Project-local notes" entry  ; {project-root}/notes.org
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
     ("pN" "Project-local notes +clipboard" entry  ; {project-root}/notes.org
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%x" :prepend t)
     ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)

     ;; Will use {org-directory}/topic/{topic-name}.
     ("o" "Templates for specific topics")
     ("ot" "Topic specific todo" entry
      (file+headline johast-org-topic-filename-select-todo "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("oT" "Topic specific todo" entry
      (file+headline johast-org-topic-filename-select-todo "Inbox")
      "* TODO %?\n%x" :prepend t)
     ("on" "Topic specific notes" entry
      (file+headline johast-org-topic-filename-select-notes "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("oN" "Topic specific notes +clipboard" entry
      (file+headline johast-org-topic-filename-select-notes "Inbox")
      "* %u %?\n%x" :prepend t)
     ("oj" "Topic specific journal" entry
      (file+olp+datetree johast-org-topic-filename-select-journal)
      "* %U %?\n%i\n%a" :prepend t)
     )
   )
  )

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

(map!
 :leader
 (:prefix "c"
  :desc "Toggle inlay hints" :n "h" #'eglot-inlay-hints-mode)
 (:prefix "j"
  :desc "Project compile" :n "C" #'johast-project-compile
  :desc "Project recompile" :n "c" #'project-recompile
  :desc "Org topic dired" :n "t" #'johast-org-topics-dired
  :desc "Org topic sync" :n "T" #'johast-org-topics-sync
  :desc "Treemacs focus" :n "p" #'treemacs-select-window)
 (:prefix "o"
  :desc "Treemacs toggle" :n "p" #'johast-treeemacs-toggle))

;; Alternative to Ctrl-I/Ctrl-O in terminal mode
;; (Ctrl-I is indistinguashible from TAB)
(global-set-key (kbd "<f3>") #'better-jumper-jump-backward)
(global-set-key (kbd "<f4>") #'better-jumper-jump-forward)
;; and then why not some buffer navigation
(global-set-key (kbd "<C-f3>") #'previous-buffer)
(global-set-key (kbd "<C-f4>") #'next-buffer)
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
 ;; RET will sometimes not only scroll line forward. Occasionally it also jumps away
 ;; when there is something at point. "t" is close to "y" so why not...
 ;; TODO: keybinding for entering that saves evil state and buffer readable state
 ;;       then enters emacs-keybinding-mode and sets file readonly.
 ;;       installs a hook when exiting view mode to also restore evil state and
 ;;       buffer readable state.
 "t" #'View-scroll-line-forward)
