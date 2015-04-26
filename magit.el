
;; Magit
(require 'magit)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'magit-find-file) ;; if not using the ELPA package
(global-set-key (kbd "C-c p") 'magit-find-file-completing-read)

(setq magit-last-seen-setup-instructions "1.4.0")
