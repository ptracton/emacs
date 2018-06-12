(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "a3132bd39a977ddde4c002f8bd0ef181414c3fbe9228e3643b999491192680ad" default)))
 '(ede-project-directories
   (quote
    ("/home/ptracton/src/software/experimental/C/STM32/STM32F3/uart_echo")))
 '(package-selected-packages
   (quote
    (flycheck-pos-tip use-package plantuml-mode smex realgud rainbow-delimiters pylint py-autopep8 paradox neotree moe-theme markdown-mode+ magit-gitflow magit-find-file json-mode jinja2-mode jedi-direx imenu-list helm-projectile helm-make helm-ls-svn helm-ls-git helm-flycheck helm-cscope helm-company graphviz-dot-mode flycheck-color-mode-line flx-ido flatland-black-theme fixmee firecode-theme elpy dracula-theme diminish diff-hl dark-krystal-theme company-math company-jedi company-c-headers company-auctex company-anaconda cherry-blossom-theme bbdb))))

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
    pos-tip
;    flycheck-pos-tip
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
;    org
;    org-projectile
;    org-magit
					;    org-plus-contrib
    bbdb
    neotree
    paradox
    anaconda-mode
    company-anaconda
    company-jedi
    company-math
    markdown-mode
    markdown-mode+
   ; google-c-style
   ; flycheck-google-cpplint
    helm-company
    helm-ls-git
    helm-ls-svn
    realgud
    flx-ido
    ;ido-ubiquitous
    diminish
    imenu-list
    plantuml-mode
    use-package
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
(global-diff-hl-mode)

;;;
;;; Paradox
;;;
(setq paradox-github-token "4deac5e8ae3ac863de01cd934cc9c78e1364d156")


;;
;; imenu https://github.com/bmag/imenu-list
;;
(imenu-list-minor-mode)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
;(setq imenu-list-auto-resize t)
(setq imenu-list-after-jump-hook nil)

;; Markdown support
(require 'markdown-mode)
(require 'markdown-mode+)
(setq markdown-command "/usr/bin/markdown")
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq markdown-css-paths `(,(expand-file-name "Documents/markdown.css")))

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

;;
;; Function to remove ^M from end of line
;;
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;
;; plantuml-mode https://github.com/skuro/plantuml-mode
;;

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

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


(setq rainbow-delimiters-mode t)

;;
;; Turn on flycheck everywhere
;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; git clone https://github.com/flycheck/flycheck-pos-tip.git
(load "~/.emacs.d/elpa/flycheck-pos-tip/flycheck-pos-tip.el")
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;;
;;; Key Bindings
;;;
(setq compilation-read-command nil)



(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "S-<f2>") 'vc-dir)
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
