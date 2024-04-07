;;; my-config/utils.el -*- lexical-binding: t; -*-

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
            (value (cdr item)))
        (when (or (not include-keys) (member key include-keys))
          (setq plist (append plist (list (intern (concat ":" key)) value))))))
    plist))

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

(provide 'my-utils)
