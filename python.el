
(elpy-enable)
;(setq elpy-rpc-backend "jedi")

;;;
;;; py-autopep8: sudo apt-get install python-autopep8
;;;
(require 'py-autopep8)
;(add-hook 'before-save-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=100"))

;;;
;;; pylint: https://bitbucket.org/logilab/pylint/pull-request/141/pylintel-missing-dependency-on-tramp/diff
;;;
(require 'tramp)
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

;;;
;;; Python Mode Key Bindings
;;; 
(add-hook 'python-mode-hook           
	  (lambda () (define-key python-mode-map (kbd "<f11>") 'py-autopep8-buffer)))
(add-hook 'python-mode-hook           
	  (lambda () (define-key python-mode-map (kbd "<f12>") 'pylint)))
