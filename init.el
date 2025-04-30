;; -*- mode: emacs-lisp -*-

;;;
;;; EMACS CONFIGURATION
;;;


;; -------------------------------
;; Basic Package Setup
;; -------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Phil Tracton"
      user-mail-address "ptracton@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Show the time and system load
(display-time-mode t)

; Show line numbers everywhere
(global-display-line-numbers-mode t)

; Show the column numbers
(column-number-mode t)

; Show the size of the file
(size-indication-mode t)

; Set the title frame to the full path to the file
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;All files have a newline at the end
(setq require-final-newline 't)

; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; turn on font-lock mode everywhere
(global-font-lock-mode t)

  ;; disable backup file creation
(setq backup-inhibited t)

; answer with y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; https://www.emacswiki.org/emacs/NoTabs
 ; Set tabs width to 4 space
(setq tab-width 4)

; Turn all tabs into spaces
(setq-default indent-tabs-mode nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

; Set the system to always use utf-8 and not ascii
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; seperate the custom variables from the handwritten sections
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; -------------------------------
;; Core Visual Enhancements
;; -------------------------------
(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package anzu
  :config (global-anzu-mode +1))

(use-package windsize
  :ensure t
  :config
  (windsize-default-keybindings)
  )

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; -------------------------------
;; Enable Shift+Arrow for Text Selection
;; -------------------------------
(setq shift-select-mode t)
(setq transient-mark-mode t)

;; -------------------------------
;; Proper Modern Shift + Arrow Selection
;; -------------------------------
;; Modern Shift + Arrow text selection without breaking Emacs keys
(use-package cua-base
  :config
  (cua-mode t)                            ;; Turn on CUA
  (cua-selection-mode t)                  ;; Only use it for text selection
  (setq cua-enable-cua-keys nil            ;; Don't overwrite C-x/C-c/C-v
        cua-enable-cursor-indications t
        cua-enable-modeline-indications t
        cua-auto-tabify-rectangles nil
        cua-keep-region-after-copy t))

;; -------------------------------
;; Completion and Navigation
;; -------------------------------
(use-package helm
  :init (helm-mode 1)
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-r" . helm-recentf)
         ("C-c h o" . helm-occur)))

(use-package helm-icons
  :after helm
  :config
  (helm-icons-enable))

(use-package treemacs
  :bind (("C-x t t" . treemacs)))

(setq treemacs-width 30)
(add-hook 'emacs-startup-hook #'treemacs)

;; -------------------------------
;; Auto-collapse Treemacs on file open
;; -------------------------------
(defun my/treemacs-maybe-collapse (&rest _)
  "Collapse Treemacs if a file is opened from Treemacs."
  (when (and (treemacs-is-treemacs-window? (selected-window))
             (buffer-file-name))
    (delete-other-windows)))

(advice-add 'treemacs-RET-action :after #'my/treemacs-maybe-collapse)

(use-package company
  :hook (prog-mode . company-mode))

;; -------------------------------
;; Syntax Checking
;; -------------------------------
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; -------------------------------
;; LSP Setup for multiple languages
;; -------------------------------
(use-package lsp-mode
  :hook ((c-mode          . my/c-cpp-setup-lsp)
         (c++-mode        . my/c-cpp-setup-lsp)
         (python-mode     . my/python-setup-lsp)
         (verilog-mode    . my/verilog-setup-lsp)
         (vhdl-mode       . my/vhdl-setup-lsp))
  :config
  (setq lsp-eldoc-render-all t
        lsp-idle-delay 0.6
        lsp-log-io nil)

  (defun my/lsp-safe-start ()
    (ignore-errors (lsp-deferred)))

  (defun my/c-cpp-setup-lsp ()
    (when (executable-find "clangd")
      (setq-local lsp-clients-clangd-executable (executable-find "clangd"))
      (my/lsp-safe-start)))

  (defun my/python-setup-lsp ()
    (cond
     ((executable-find "pyright-langserver")
      (require 'lsp-pyright)
      (my/lsp-safe-start))
     ((executable-find "pylsp")
      (my/lsp-safe-start))))

  (defun my/verilog-setup-lsp ()
    (cond
     ((executable-find "svlangserver")
      (setq-local lsp-verilog-server 'svlangserver)
      (my/lsp-safe-start))
     ((executable-find "verible-verilog-ls")
      (setq-local lsp-verilog-server 'verible-verilog-ls)
      (setq lsp-verilog-verible-verilog-ls-binary (executable-find "verible-verilog-ls"))
      (my/lsp-safe-start))))

  (defun my/vhdl-setup-lsp ()
    (when (executable-find "ghdl-ls")
      (my/lsp-safe-start))))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :if (executable-find "pyright-langserver")
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package verilog-mode)
(use-package vhdl-mode)

;; -------------------------------
;; Formatting
;; -------------------------------
(use-package reformatter)

(reformatter-define verilog-format
  :program "verible-verilog-format"
  :args '("--stdin-filepath" buffer-file-name "--fallback-style" "Google")
  :lighter " VF")

(add-hook 'verilog-mode-hook #'verilog-format-on-save-mode)

(defun my/vhdl-format-on-save ()
  (add-hook 'before-save-hook #'vhdl-beautify nil t))

(add-hook 'vhdl-mode-hook #'my/vhdl-format-on-save)

(reformatter-define black-format
  :program "black"
  :args '("-q" "-")  ;; quiet, read from stdin
  :group 'python
  :lighter " ⬛")

(add-hook 'python-mode-hook #'black-format-on-save-mode)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; -------------------------------
;; UI and Dashboard
;; -------------------------------

;; Highlight current line
(global-hl-line-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-icon t))

;; All-the-icons needed for doom-modeline
(use-package all-the-icons
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                           (projects . 5)))
  (dashboard-setup-startup-hook))

;; -------------------------------
;; Org Mode
;; -------------------------------
(use-package org
  :hook (org-mode . visual-line-mode))

;; -------------------------------
;; Org-Mode Power Tools
;; -------------------------------

;; Org base
(setq org-directory "~/org")
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Org-agenda
(setq org-agenda-files (list org-directory))
(global-set-key (kbd "C-c a") 'org-agenda)

;; Org-journal
(use-package org-journal
  :custom
  (org-journal-dir (expand-file-name "journal/" org-directory))
  (org-journal-file-type 'daily)
  (org-journal-enable-agenda-integration t)
  :bind
  ("C-c j" . org-journal-new-entry))

;; Org-roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t) ; suppress v2 upgrade warning
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :config
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; -------------------------------
;; Embedded Systems tools
;; -------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'my-embedded)
(require 'my-devtools)

;; -------------------------------
;; Suppress Annoying Warning Buffers
;; -------------------------------

(setq warning-minimum-level :error)

;; Alternative (even stricter)
;; (setq warning-minimum-level :emergency)


;; -------------------------------
;; Theme and Fonts
;; -------------------------------

;; Doom Themes
(use-package doom-themes
  :init
  (load-theme 'doom-one t)) ;; Or doom-dracula, doom-gruvbox, etc.

;; Fonts
(set-face-attribute 'default nil
                    :font "Fira Code Retina"
                    :height 120) ;; 12pt size (120 = 12pt * 10)

;; Enable font ligatures if you want (optional bonus)
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(set-background-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function to remove ^M from end of line
;; http://stackoverflow.com/questions/730751/hiding-m-in-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c C-g") 'projectile-grep)

(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "S-<f2>") 'vc-dir)
(global-set-key (kbd "<f3>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f4>") 'rainbow-delimiters-mode)
(global-set-key (kbd "<f5>") 'comment-or-uncomment-region)
(global-set-key (kbd "<f6>") 'highlight-changes-visible-mode)
(global-set-key (kbd "S-<f6>") 'highlight-changes-remove-highlight)
(global-set-key (kbd "<f7>") 'whitespace-mode)
(global-set-key (kbd "<f8>") 'python-black-buffer)
(global-set-key (kbd "<f9>") 'flycheck-list-errors)
(global-set-key (kbd "<f10>") 'org-capture)
(global-set-key (kbd "S-<f10>") 'org-agenda)
(global-set-key (kbd "<f11>") 'lsp-treemacs-symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Associations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.stim\\'" . verilog-mode))
  (add-to-list 'auto-mode-alist  '("\\.vh\\'" . verilog-mode))
  (add-to-list 'auto-mode-alist '("\\.f\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  )


(provide 'init)
;;; init.el ends here
