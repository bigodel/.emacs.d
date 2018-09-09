
(setq flyspell-issue-message-flag nil)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)

(defun /flyspell/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "brazilian")
                     "english"
                   "brazilian")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "M-<f8>") #'/flyspell/switch-dictionary)
