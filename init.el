;;;
;;; EMACS CONFIGURATION
;;;

(setq package-check-signature nil)

;;; CODE:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;;;
;;; List of packages to install and use
;;;
(defconst demo-packages
  '(
    use-package
    tramp
    windsize
    imenu-list
    elpy
    py-autopep8
    pylint
    projectile
    treemacs-projectile
    helm
    helm-flycheck
    helm-projectile
    helm-gtags
    helm-xref
    org
    magit
    diff-hl
    smex
    rainbow-delimiters
    dracula-theme
    imenu
    lsp-mode
    lsp-ui
    lsp-treemacs
;    treemacs-magit
;    treemacs-icons-dired
    helm
    helm-lsp
    hydra
    flycheck
    flycheck-color-mode-line
    plantuml-mode
    company
    company-box
    company-lsp
    auctex
    anzu))

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
(require 'recentf)
(recentf-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)

(global-visual-line-mode 1) ;; how long lines are handled.  This
                            ;; appears to wrap long lines visually,
                            ;; but not add line-returns

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

;;; https://www.emacswiki.org/emacs/NoTabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flycheck
;; https://www.flycheck.org/en/latest/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(global-flycheck-mode t)


(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-keymap-prefix "C-l")
(require 'lsp-mode)
(require 'lsp-clients)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'perl-mode-hook #'lsp)
(add-hook 'verilog-mode-hook #'lsp)
(add-hook 'vhdl-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'asm-mode-hook #'lsp)
(add-hook 'tex-mode-hook #'lsp)
(add-hook 'json-mode-hook #'lsp)
(setq lsp-prefer-capf t)
(setq lsp-idle-delay 0.500)

;;
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;;
(use-package lsp-mode
  ;; ..

  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.

  ;; ..
  )

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


(use-package helm)

(use-package helm-lsp
  :config
  (defun netrom/helm-lsp-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-workspace-symbol)))

  (defun netrom/helm-lsp-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-global-workspace-symbol))))


(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ANZU
;; https://github.com/syohex/emacs-anzu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; imenu
;; https://github.com/bmag/imenu-list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(imenu-list-minor-mode)
;;(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
                                        ;(setq imenu-list-auto-resize t)
;;(setq imenu-list-after-jump-hook nil)
;;(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(lsp-treemacs-sync-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; windsize
;; https://github.com/grammati/windsize
;;
;; Then use C-S-<left>, C-S-<right>, C-S-<up>, and C-S-<down> to move window edges.
;; Resizes by 8 columns or 4 rows by default. Change that by setting windsize-cols and/or windsize-rows.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'windsize)
(windsize-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ORG Mode
;; https://orgmode.org/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MAGIT
;; https://magit.vc/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)

;; https://pages.sachachua.com/.emacs.d/Sacha.html#orgd6b0c8c
(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DIFF HL
;; https://github.com/dgutov/diff-hl
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-diff-hl-mode)
(advice-add 'svn-status-update-modeline :after #'diff-hl-update)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;; https://projectile.readthedocs.io/en/latest/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-switch-project-action 'projectile-dired)


(use-package treemacs
  :hook
  ;; slightly lower the size of treemacs window
  ((treemacs-mode . (lambda ()
                      (setq buffer-face-mode-face '(:height .88))
                      (buffer-face-mode))))
  :config
  (progn
    (setq treemacs-follow-after-init t
          treemacs-recenter-after-file-follow t
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-eldoc-display nil
          treemacs-collapse-dirs (if (executable-find "python") 3 0)
          treemacs-silent-refresh t
          treemacs-silent-filewatch t
          treemacs-change-root-without-asking t
          treemacs-sorting 'alphabetic-asc
          treemacs-show-hidden-files t
          treemacs-never-persist nil
          treemacs-is-never-other-window t)

    ;; set the correct python3 executable path. This is needed for
    ;; treemacs-git-mode extended
    (setq treemacs-python-executable (executable-find "python"))

    ;; highlight current line in fringe for treemacs window
    (treemacs-fringe-indicator-mode)

    (defun doom-themes-hide-modeline ()
      (setq mode-line-format nil))

    ;; The modeline isn't useful in treemacs
    (add-hook 'treemacs-mode-hook #'doom-themes-hide-modeline)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-resize-icons 16)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

  :bind
  (:map global-map
        ("C-c f" . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (:map global-map
              ("C-c o t" . treemacs-projectile)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; Integration between lsp-mode and treemacs and implementation of treeview
;; controls using treemacs as a tree renderer.
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs :defer 3
  :config
  (lsp-treemacs-sync-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HELM
;; https://emacs-helm.github.io/helm/
;; http://tuhdo.github.io/helm-intro.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

; https://github.com/bbatsov/helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(require 'helm-flycheck) ;; Not necessary if using ELPA package
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))


(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

  ;; helm-M-x
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; helm-kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files);

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-locate-fuzzy-match t)
(global-set-key (kbd "C-c h o") 'helm-occur)

;;
;; https://github.com/syohex/emacs-helm-gtags
;;

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'makefile-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
(add-hook 'verilog-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python Programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(elpy-enable)

(setq python-indent-offset 4)

;; https://realpython.com/emacs-the-best-python-editor/
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;
;;; pylint: https://bitbucket.org/logilab/pylint/pull-request/141/pylintel-missing-dependency-on-tramp/diff
;;;
(require 'tramp)
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq flycheck-python-pylint-executable "/user/tractp1/links/scratch/anaconda3/bin/pylint")
;;             (setq flycheck-pylintrc "~/.pylintrc"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C Programming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(semantic-mode 1)
(global-ede-mode 1)                      ; Enable the Project management system
(setq c-basic-indent 4)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(global-semantic-idle-scheduler-mode  1)
(global-semantic-idle-completions-mode  1)
(global-semantic-idle-summary-mode  1)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(add-hook 'c-mode-common-hook   'hs-minor-mode)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))


(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
					'(("\\<\\(REPAIRED\\|STUBBED\\):" 1 font-lock-keyword-face t)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Function to remove ^M from end of line
;;
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keys
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "S-<f2>") 'vc-dir)
(global-set-key (kbd "<f3>") 'find-tag)
(global-set-key (kbd "<f4>") 'rainbow-delimiters-mode)
(global-set-key (kbd "<f5>") 'comment-or-uncomment-region)
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode)
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'py-autopep8-buffer)
(global-set-key (kbd "<f9>") 'pylint)
(global-set-key (kbd "<f10>") 'flycheck-list-errors)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File Associations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.stim\\'" . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.vh\\'" . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set Theme and Colors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dracula-theme)
(load-theme 'dracula)

(set-background-color "black")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auctex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" default)))
 '(lsp-treemacs-sync-mode nil)
 '(package-selected-packages
   (quote
    (company-box treemacs-icons-dired treemacs-magit treemacs-projectile helm-imenu lsp-treemacs plantuml-mode windsize use-package smex rainbow-delimiters pylint py-autopep8 magit lsp-ui imenu-list hydra helm-xref helm-projectile helm-lsp helm-gtags helm-flycheck flycheck-color-mode-line elpy dracula-theme diff-hl company-lsp auctex anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
