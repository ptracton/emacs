(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "a3132bd39a977ddde4c002f8bd0ef181414c3fbe9228e3643b999491192680ad" default)))
 '(ede-project-directories
   (quote
    ("/home/ptracton/src/software/experimental/C/STM32/STM32F3/uart_echo"))))
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
             '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)


;;; 
;;; List of packages to install and use
;;;
(defconst demo-packages
  '(moe-theme
    dark-krystal-theme
    flatland-black-theme
    dracula-theme
    cherry-blossom-theme
    firecode-theme
;    auto-complete  ;Needed for auto-complete-verilog.el,
    cl
    elpy
    pylint
    py-autopep8
    jedi
    jedi-direx
    tramp
    smex
    auctex
    graphviz-dot-mode
    jinja2-mode       ;http://github.com/paradoxxxzero/jinja2-mode
    json-mode
    xcscope
    button-lock
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
    org-plus-contrib
    neotree
    paradox
    anaconda-mode
    company-anaconda
    company-jedi
    company-math
    google-c-style
    flycheck-google-cpplint
    helm-company
    helm-ls-git
    helm-ls-svn
    realgud
    flx-ido
    ido-ubiquitous
    diminish
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
(setq show-paren-delay 0)
(show-paren-mode 1)

(global-visual-line-mode 1) ;; how long lines are handled.  This
                            ;; appears to wrap long lines visually,
                            ;; but not add line-returns

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

;;;
;;; Paradox
;;;
(setq paradox-github-token "4deac5e8ae3ac863de01cd934cc9c78e1364d156")

;;;
;;; Company Mode for auto-completion
;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-math-symbols-unicode)

;;;
;;; YASnippet everywhere
;;;
(require 'yasnippet)
(yas-global-mode 1)

;;;
;;; Fixmee
;;;
(require 'button-lock)
(global-button-lock-mode 1)

(require 'fixmee)
(global-fixmee-mode 1)

;;;
;;; https://github.com/nonsequitur/smex
;;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; diminish keeps the modeline tidy
(require 'diminish)

;; bookmarks
(require 'bookmark)

;;;
;;;
;;;
(load-file "~/.emacs.d/util.el")

;;;
;;; IDO
;;;
(load-file "~/.emacs.d/ido.el")


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
(global-set-key (kbd "<f9>") 'fixmee-view-listing)
;(global-set-key (kbd "<f9>") 'nlinum-mode)
(global-set-key (kbd "<f10>") 'flycheck-list-errors)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(progn
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
  )


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;
;;; Set up colors and appearance
;;; https://github.com/kuanyui/moe-theme.el
;;;

;(require 'moe-theme)
;(load-theme 'moe-dark t)

;(require 'cherry-blossom-theme)
;(load-theme 'cherry-blossom)

;(require 'dark-krystal-theme)
;(load-theme 'dark-krystal)

;(require 'flatland-black-theme)
;(load-theme 'flatland-black)

;(require 'firecode-theme)
;(load-theme 'firecode)


(require 'dracula-theme)
(load-theme 'dracula)

(set-background-color "black")
