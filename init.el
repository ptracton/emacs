(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("a3132bd39a977ddde4c002f8bd0ef181414c3fbe9228e3643b999491192680ad" default)))
 '(ede-project-directories (quote ("/home/ptracton/src/software/experimental/C/STM32/STM32F3/uart_echo"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;
;;; EMACS CONFIGURATION
;;;

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(setq user-mail-address "ptracton@gmail.com")
(setq user-full-name "Philip Tracton")

(defalias 'yes-or-no-p 'y-or-n-p)

;;; 
;;; List of packages to install and use
;;;
(defconst demo-packages
  '(moe-theme
    dark-krystal-theme
    auto-complete  ;Needed for auto-complete-verilog.el,
    cl
    elpy
    pylint
    py-autopep8 
    jedi
    tramp    
    smex
    auctex
    graphviz-dot-mode
    jinja2-mode       ;http://github.com/paradoxxxzero/jinja2-mode
    json-mode
    xcscope
    fixmee
    flycheck
    flycheck-color-mode-line
    helm
    helm-cscope
    helm-flycheck
    helm-make
    helm-projectile
    rainbow-delimiters
    company
    company-c-headers
    company-auctex
    projectile
    org
    org-projectile
    org-magit
    magit
    magit-gitflow
    magit-find-file))


;;;
;;; Function to install packages
;;;
(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

;;;
;;; Actually install the packages 
;;;
(install-packages)

;;;
;;; General Setup
;;;
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)
(setq inhibit-startup-screen t)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq require-final-newline 't)
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
(recentf-mode 1)

;;;
;;; Company Mode for auto-completion
;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;
;;; YASnippet everywhere
;;;
(require 'yasnippet)
(yas-global-mode 1)

;;;
;;; Fixmee
;;; 
(require 'fixmee)
;(global-fixmee-mode 1)

;;;
;;; https://github.com/nonsequitur/smex
;;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;
;;; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html
;;;
(global-highlight-changes-mode t)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#382f2f")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;;;
;;; No tabs but use 4 spaces for a tab key
;;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;
;;; Magit http://magit.vc/
;;;
(load-file "~/.emacs.d/magit.el")

;;;
;;; Python
;;;
(load-file "~/.emacs.d/python.el")

;;;
;;; Verilog
;;;
;(load-file "~/.emacs.d/verilog.el")

;;;
;;; C 
;;;
(load-file "~/.emacs.d/c.el")
;;;
;;; LaTex Mode 
;;;
(load-file "~/.emacs.d/latex.el")

;;;
;;; ORG Mode 
;;;
(load-file "~/.emacs.d/org.el")

;;;
;;; Projectile Mode 
;;;
(load-file "~/.emacs.d/projectile.el")

;;;
;;; Helm Mode 
;;;
(load-file "~/.emacs.d/helm.el")

;;;
;;; Key Bindings
;;;
(setq compilation-read-command nil)
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "<f4>") 'rainbow-delimiters-mode)
(global-set-key (kbd "<f5>") 'recentf-open-files)
(global-set-key (kbd "S-<f5>")  'highlight-changes-previous-change)
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode) 
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'fixmee-view-listing)
;(global-set-key (kbd "<f9>") 'nlinum-mode)
(global-set-key (kbd "<f10>") 'flycheck-list-errors)


;;;
;;; Set up colors and appearance
;;; https://github.com/kuanyui/moe-theme.el
;;;
;(require 'moe-theme)
;(load-theme 'moe-dark t)
(require 'dark-krystal-theme)
(load-theme 'dark-krystal)
(set-background-color "black")

;;;
;;; END OF EMACS CONFIGURATION
;;;

