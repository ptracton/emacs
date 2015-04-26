(require 'helm-config)
(helm-mode 1)

 (require 'helm-flycheck) ;; Not necessary if using ELPA package
 (eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
