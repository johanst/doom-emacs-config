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
