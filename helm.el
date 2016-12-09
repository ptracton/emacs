(require 'helm-config)
(helm-mode 1)

 (require 'helm-flycheck) ;; Not necessary if using ELPA package
 (eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(require 'helm-ls-svn)
(require 'helm-ls-git)
(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
