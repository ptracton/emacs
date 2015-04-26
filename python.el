
(elpy-enable)
;(setq elpy-rpc-backend "jedi")

;;;
;;; py-autopep8: sudo apt-get install python-autopep8
;;;
;(load-file "~/.emacs.d/elpa/py-autopep8.el/py-autopep8.el")
;(require 'py-autopep8)
;(add-hook 'before-save-hook 'py-autopep8-enable-on-save)
;(setq py-autopep8-options '("--max-line-length=100"))

;;;
;;; pylint: https://bitbucket.org/logilab/pylint/pull-request/141/pylintel-missing-dependency-on-tramp/diff
;;;
(require 'tramp)
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

;;;
;;; Anaconda Mode
;;;
;(add-hook 'python-mode-hook 'anaconda-mode)

;;;
;;; Jedi setup http://tkf.github.io/emacs-jedi/latest/#faq
;;;
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(defun run-python-script () 
  (interactive)
  (shell-command (format "python %s" (buffer-name)) "*python-output*"))
(global-set-key [f12] 'run-python-script)


;;;
;;; nose testing
;;; 
;(require 'nose)
;(add-hook 'python-mode-hook (lambda () (nose-mode t)))
