;;; testing.el -*- lexical-binding: t; -*-
;;;
;;; -- Various things in progress --

(defun env-alist-from-key-value-file(file)
  "Given a file containing lines like KEY=value, produce a corresponding alist
like ((\"KEY1\" . \"value1\") (\"KEY2\" . \"value2\"))"
  (interactive "f.env file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((alist '()))
      (while (re-search-forward "^\\([^=\n]+\\)=\"?\\([^\"\n]+\\)\"?$" nil t)
        (push `(,(match-string 1) . ,(match-string 2)) alist))
      alist)))

(defun env-get-process-environment-from-alist(alist &optional include-keys)
  "Given an alist with KEY/VALUE elements, combine the existing
process-environment with the alist so that the alist values adds
or replaces lines like 'KEY=VALUE' that can later on be used to
set the process-environment.

'include-keys' is a list of strings that specifies which keys
from the alist that should be used. may be used to filter only
specific keys. nil means every key is accepted."
  (interactive "xgimme an alist: ")
  (let ((env (copy-sequence process-environment)))
    (dolist (item alist)
      (let ((key (car item))
            (value (cdr item)))
        (when (or (not include-keys) (member key include-keys))
          (progn
            (setq env
                  (cl-remove-if
                   (lambda(x) (string-prefix-p (concat key "=") x)) env))
            (push (concat key "=" value) env)))))
    env))

(defun env-plist-from-alist(alist &optional include-keys)
  "Given an alist with KEY/VALUE elements, produce a plist like
(:KEY1 \"value\" :KEY2 \"value2\"). It happens to be just what eglot and
dape wants for use in its jsonrpc requests.

'include-keys' is a list of strings that specifies which keys
from the alist that should be used. may be used to filter only
specific keys. nil means every key is accepted."
  (interactive "xgimme an alist: ")
  (let ((plist '()))
    (dolist (item alist)
      (let ((key (car item))
            (val (cdr item)))
        (when (or (not include-keys) (member key include-keys))
          (setq plist (append plist (list (intern (concat ":" key)) val))))))
    plist))


(dolist (item alist)
      (let ((key (car item))
            (val (cdr item)))
        (when t
          (setq knas )
          (setq gurka val))))))

(env-plist-from-alist
               '(("WSLENV" . "apa")
                 ("EDITOR" . "emacs")
                 ("TMUX_PANE" . "jupp")
                 )
               '("WSLENV" "EDITOR")
               )

(env-get-process-environment-from-alist
               '(("WSLENV" . "apa")
                 ("EDITOR" . "emacs")
                 ("TMUX_PANE" . "jupp")
                 )
               '("WSLENV" "EDITOR")
               )

(setq test-plist '(:key1 "v1" :key2 "v2"))
(setq mykey :key1)
(plist-get test-plist mykey)
(setq knas '("KWHCALC_BUILD_ENV" . "NATIVE"))

     (setq njatt
      '(("RUSTFLAGS" . "--target aarch64-unknown-linux-gnu"))
      )

      (setq-local
        njett
        `(:native
          ,(env-plist-from-alist njatt)
          )
        )
;; global johast-eglot-config
;; :rust-build-env alist/env-file ( native or rustic build +  )
;;    :build-env for eglot (architecture, path to cargo etc.)
;; :rust-run-env (native rustic test + run )
;; :debug-env (native or cross)
;;   rustic-cargo-bin
;;   rustic-cargo-build-exec-command
;;   rustic-cargo-build-arguments
;;
;; rustic run-command (that always need native run + build env):
;;   rustic-cargo-test-run
;;   rustic-cargo-run-test
;;   rustic-cargo-run
;; rustic build-command (that only builds so should use native|target buildenv)
;; all ends in rustic-compilation-start...
;;
;; apparently we need inheritenv from envrc
(setq my-list (cl-remove-if (lambda (x) (and (stringp x) (string-prefix-p prefix-to-remove x))) my-list))
(message "%s" (env-get-process-environment-from-alist '((2 . 3))))
(message "%s" (env-alist-from-key-value-file "key_value.txt"))

(defun plist-from-key-value-file (file)
  "Construct a plist from a key-value file."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((plist '()))
      (while (re-search-forward "\\([^=]+\\)=\\(.+\\)$" nil t)
        (setq plist (cons (intern (match-string 1)) (cons (match-string 2) plist)))))
      (apply 'plist-put plist)))
;; Example usage:
(setq my-plist (plist-from-key-value-file "key_value.txt"))

`(,(intern (concat ":" "hej")) "podaj")

(defvar-keymap johast-moving-repeat-map
  :doc "Keymap to repeat `next-buffer' and `previous-buffer'.  Used in `repeat-mode'."
  :repeat t
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-list
  "p" #'backward-list
  "u" #'backward-up-list
  "d"  #'down-list)

;; johast-build-environment
;; johast-run-environment

(setq alist '())
(add-to-list 'alist '(2 . 3))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-unfold-all))
  (if (+fold--ts-fold-p)
      (ts-fold-open-all)
    (save-excursion
      (+fold--ensure-hideshow-mode)
      (if (integerp level)
          (progn
            (outline-hide-sublevels (max 1 (1- level)))
            (hs-life-goes-on
             (hs-hide-level-recursive (1- level) (point-min) (point-max))))
        (hs-show-all)
        (when (fboundp 'outline-show-all)
          (outline-show-all))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (when (featurep 'vimish-fold)
    (vimish-fold-refold-all))
  (save-excursion
    (if (+fold--ts-fold-p)
        (ts-fold-close-all)
      (progn
        (+fold--ensure-hideshow-mode)
        (hs-life-goes-on
         (if (integerp level)
             (progn
               (outline--show-headings-up-to-level (1+ level))
               (hs-hide-level-recursive (1- level) (point-min) (point-max)))
           (hs-hide-all)
           (when (fboundp 'outline-hide-sublevels)
             (outline-show-only-headings))))))))


;; https://www.reddit.com/r/emacs/comments/12g30nz/keybindings_for_info_when_using_evil/
(defun ev/evil-collection-visit-file (&optional mode)
    "Visit the `evil-collection' source file corresponding to
  the current major mode, or to MODE if specified, in a new
  window."
    (interactive)
    (let* ((mode (or mode (format "%s" major-mode)))
           (base-path (concat (file-name-directory (find-library-name "evil-collection")) "modes/"))
           (short (replace-regexp-in-string "-.+$" "" mode))
           (file-list (directory-files-recursively base-path (format ".*%s.*\\.el$" short)))
           (choice (cond ((= (length file-list) 1)
                          (car file-list))
                         ((> (length file-list) 1)
                          (completing-read "Multiple evil-collection files found. Visit: " file-list nil t))
                         (t
                          (user-error "No evil-collection file found for %s" (format "%s" mode))))))

        (select-window (display-buffer (find-file-noselect choice)
                                       '(display-buffer-at-bottom . ())))
      (let ((scroll-preserve-screen-position t)
            (cur-line (line-number-at-pos))
            (start (line-number-at-pos (window-start))))

        (beginning-of-buffer)
        (re-search-forward "(evil-collection-define-key")
        (beginning-of-line)
        (scroll-up (- cur-line start)))))
