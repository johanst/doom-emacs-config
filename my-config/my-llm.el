;;; my-config/my-llm.el -*- lexical-binding: t; -*-
(require 'doom-lib)

(declare-function gptel-make-anthropic gptel)
(declare-function gptel-make-gh-copilot gptel)
(defvar gptel-org-branching-context)

(declare-function gptel-quick gptel-quick)
(defvar gptel-quick-use-context)
(defvar gptel-quick-system-message)

(declare-function minuet-set-optional-options minuet)

(after! gptel
  (setq
   gptel-default-mode 'org-mode
   gptel-model 'gpt-4.1
   gptel-backend (gptel-make-gh-copilot "Copilot")
   gptel-org-branching-context t
   )
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (add-hook 'gptel-mode-hook (lambda () (make-local-variable 'gptel-context--alist)))
  )

(defun my-gptel-use-claude ()
  (interactive)
  (require 'gptel)
  (setq
   gptel-model 'claude-3-5-sonnet-20241022
   gptel-backend
   (gptel-make-anthropic "Claude"
     :stream t
     :key (nth 0 (process-lines "pass" "show" "anthropic/apikey")))))

(defun my-gptel-quick-with-context ()
  (interactive)
  (let ((old-context gptel-quick-use-context))
    (setq gptel-quick-use-context t)
    (save-excursion
      (call-interactively #'gptel-quick))
    (setq gptel-quick-use-context old-context)))

(defvar my-gptel-default-quick-system-message nil
  "A variable that holds the default system message from gptel.")

(defvar my-gptel-skånska-quick-system-message
  (lambda (count)
    (concat (format "Explain in %d words or fewer. " count )
            "Use swedish colloquial language and spelling with accent skånska. "
            "Try to use as much skånska as possible without losing clarity."))
  "A variable that holds the skånska system message from gptel.")

(defvar my-gptel-skånska nil
  "Whether gptel should talk skånska.")

(defun my-gptel-toggle-skånska ()
    "Toggle skånska"
    (interactive)
    (if my-gptel-skånska
        (progn
          (setq my-gptel-skånska nil)
          (message "Being international now.")
          (when my-gptel-default-quick-system-message
            (setq gptel-quick-system-message my-gptel-default-quick-system-message)))
      (progn
          (setq my-gptel-skånska t)
          (message "Förklarar nu på redig skånska.")
          (when my-gptel-default-quick-system-message
            (setq gptel-quick-system-message my-gptel-skånska-quick-system-message)))))

(after! gptel-quick
  (setq
   gptel-quick-timeout 120 ;; 2 minutes, but why would i ever want a timeout ?
   gptel-quick-word-count 30 ;; I would rarely want less than 30 words in an explanation
   my-gptel-default-quick-system-message gptel-quick-system-message
   )
  (when my-gptel-skånska
    (setq gptel-quick-system-message my-gptel-skånska-quick-system-message))
  )

(defun my-mistral-endpoint-set-default ()
  "Set the mistral endpoint to the default mistral cloud hosted endpoint.
Also configure the API-key from `pass show mistral/apikey'."
  (interactive)
  (plist-put minuet-codestral-options :end-point "https://api.mistral.ai/v1/fim/completions")
  (plist-put minuet-codestral-options :model "codestral-latest")
  (setq my-mistral-api-key (nth 0 (process-lines "pass" "show" "mistral/apikey"))))

(defvar my-mistral-api-key nil
  "A variable that holds the mistral api-key.")
(defun my-mistral-api-key ()
  "Return the mistral api-key from the variable `my-mistral-api-key'.
If the variable `my-mistral-api-key' is not set, it will be set to the output of
the command `pass show mistral/apikey'."
  (interactive)
  (when (not my-mistral-api-key)
    (my-mistral-endpoint-set-default))
  my-mistral-api-key)

(use-package! plz)
(use-package! minuet
  :defer t

  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
   ("C-c a" . #'minuet-auto-suggestion-mode) ;; toggle auto suggestion)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-a" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-A will accepts 2 lines of completion.
   ("M-A" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-provider 'codestral)
  (plist-put minuet-codestral-options :api-key #'my-mistral-api-key)
  (plist-put minuet-codestral-options :end-point "https://api.mistral.ai/v1/fim/completions")
  (minuet-set-optional-options minuet-codestral-options :max_tokens 64)
  (minuet-set-optional-options minuet-codestral-options :temperature 0)
  )

(provide 'my-llm)
