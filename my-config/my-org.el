;;; my-config/my-org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-ellipsis " ▼"))

(defvar my-org-topic-dir
  (expand-file-name "topic" org-directory)
  "Directory for recording topic specific notes/todos/journal")

(defun my-org-topics-list ()
  "Return a list of topics handled by org"
  (if (file-exists-p my-org-topic-dir)
      (cl-remove-if (lambda (x) (member x '("." "..")))
                    (directory-files my-org-topic-dir))
    nil))

(defun my-org-topic-filename-select (filename)
  "Query user for a topic in org topic directory and return full path to
   filename within that directory"
  (let ((topic-filename
         (concat (file-name-as-directory my-org-topic-dir)
                 (file-name-as-directory (completing-read "Select topic: " (my-org-topics-list)))
                 filename)))
    topic-filename))

(defun my-org-topic-filename-select-todo ()
  "Select org topic and return full path to its todo file"
  (my-org-topic-filename-select "todo.org"))

(defun my-org-topic-filename-select-notes ()
  "Select org topic and return full path to its notes file"
  (my-org-topic-filename-select "notes.org"))

(defun my-org-topic-filename-select-journal ()
  "Select org topic and return full path to its journal file"
  (my-org-topic-filename-select "journal.org"))

(defun my-org-topics-dired ()
  "Open dired in org-directory/topic for quick add/remove of directories that
   will correspond to topics "
  (interactive)
  (dired my-org-topic-dir))

(defun my-topicdirp (dirname)
  "Return t if parent directory is topic"
  (string-equal
   "topic"
   (file-name-base
    (directory-file-name (file-name-directory (expand-file-name dirname)))
    )))

(defun my-org-topics-sync ()
  "Update org-agenda-files to match contents of org-directory/topic"
  (interactive)
  (setq
   org-agenda-files
   (cons org-directory
         (when (file-exists-p my-org-topic-dir)
           (directory-files-recursively
            my-org-topic-dir
            "\\.org$"
            t
            #'my-topicdirp))))
  (message "Synchronized topic dir '%s'" my-org-topic-dir))

(after! org
  ;; Open org links in other window
  (add-to-list 'org-link-frame-setup (cons 'file 'find-file-other-window))

  ;; Org-cache seems to be messed up when treemacs is parsing the
  ;; projects and workspaces
  (setq org-element-use-cache nil)

  (my-org-topics-sync)
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
      (file+headline my-org-topic-filename-select-todo "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("oT" "Topic specific todo" entry
      (file+headline my-org-topic-filename-select-todo "Inbox")
      "* TODO %?\n%x" :prepend t)
     ("on" "Topic specific notes" entry
      (file+headline my-org-topic-filename-select-notes "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("oN" "Topic specific notes +clipboard" entry
      (file+headline my-org-topic-filename-select-notes "Inbox")
      "* %u %?\n%x" :prepend t)
     ("oj" "Topic specific journal" entry
      (file+olp+datetree my-org-topic-filename-select-journal)
      "* %U %?\n%i\n%a" :prepend t)
     )
   )
  )

(provide 'my-org)
