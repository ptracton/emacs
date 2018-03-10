;;* ido completion
(require 'ido)
;(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10

      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
;(ido-ubiquitous-mode +1)

;;** smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/
(require 'ido-hacks nil t)
(if (commandp 'ido-vertical-mode)
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))
