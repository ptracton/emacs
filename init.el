;;; init.el --- Phil Tracton's Gemini-IDE (Mint 22.1 / Emacs 29.3)

;; =============================================================================
;; QUICK REFERENCE: ORG-MODE, KNOWLEDGE & CALENDAR
;; =============================================================================
;;  TASKS & AGENDA:
;;    C-c a       : Open Agenda (Daily/Weekly view)
;;    C-c c       : Capture (Quick add Task or Log entry)
;;    Tab         : Cycle TODO -> DONE (within an .org file)
;;    C-c [       : Add current file to the Agenda list
;;    C-c C-s     : Schedule a task (Adds to Calendar/Agenda)
;;    C-c C-d     : Set a Deadline
;;
;;  JOURNALING & TIME TRACKING:
;;    C-c c l     : Daily Engineering Log (Auto-dates and files)
;;    C-c c t     : Quick Task (Enters into your global Inbox)
;;    C-c C-x C-i : Clock In (Start timer on current task)
;;    C-c C-x C-o : Clock Out (Stop timer)
;;
;;  ORG-ROAM (Zettelkasten / Knowledge Graph):
;;    C-c n f     : Find or Create a Node (e.g., "Verilog-Best-Practices")
;;    C-c n i     : Insert a Link to another Node (Links them in the Graph)
;;    C-c n l     : Toggle Roam Buffer (See what links TO this file)
;;    C-c n g     : Graph View (See your brain in a web browser)
;;
;; =============================================================================
;; QUICK REFERENCE: PROJECT, LSP & EDITING
;; =============================================================================
;;  F1 : Recent Files (Helm)      F2 : Git Status (Magit)
;;  F5 : Project Build (Make)     F6 : Toggle Git Blame (Inline)
;;  C-c p s : Ripgrep Project     C-c p f : Find File in Project
;;  C-x t t : Toggle Treemacs     C-c l s : LSP Symbol Tree (Outline)
;;
;;  EDITING & FORMATTING:
;;  F3 : Clean WhiteSpace         F4 : Toggle Rainbow Delims
;;  C-a : Smart Home (Crux)       C-c i : Indent Entire Buffer
;;  C-c t : Toggle HDL (.v/.vh)   M-s   : Avy Jump (Quick Jump)
;;  C-= / C-- : Font Scale Up/Dn  C-> / C-< : Multiple Cursors
;; =============================================================================

;; --- 1. User Identity & Typography ---
(setq user-full-name "Phil Tracton" user-mail-address "ptracton@gmail.com")
(set-face-attribute 'default nil :family "Fira Code" :height 140)

(global-set-key (kbd "C-=") (lambda () (interactive) (let ((s (+ (face-attribute 'default :height) 10))) (set-face-attribute 'default nil :height s))))
(global-set-key (kbd "C--") (lambda () (interactive) (let ((s (- (face-attribute 'default :height) 10))) (set-face-attribute 'default nil :height s))))

;; --- 2. Performance & Path Inheritance ---
(use-package gcmh :ensure t :init (gcmh-mode 1)) 
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(x pgtk))
  :config (exec-path-from-shell-initialize))

(setq read-process-output-max (* 1024 1024) inhibit-startup-message t make-backup-files nil require-final-newline t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil tab-width 4 c-basic-offset 4)

;; --- 3. UI, Themes & Line Management ---
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)
(scroll-bar-mode 1) (tool-bar-mode 1) (menu-bar-mode 1) (column-number-mode t)

(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t)
  (set-face-background 'default "#000000")
  (set-face-background 'line-number "#000000")
  (set-face-background 'fringe "#000000")
  (set-face-background 'hl-line "#1a1a1a"))

(use-package doom-modeline :init (doom-modeline-mode 1))
(use-package anzu :config (global-anzu-mode +1) (setq anzu-cons-mode-line-p nil))
(use-package which-key :init (which-key-mode) :config (setq which-key-idle-delay 0.3))

;; --- 4. Package Management ---
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

;; --- 5. Helm & Project Search ---
(use-package helm
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x) ("C-x C-f" . helm-find-files) ("C-x b" . helm-buffers-list) ("<f1>" . helm-recentf))
  :config (setq helm-split-window-inside-p nil helm-display-buffer-default-height 0.3))
(add-to-list 'display-buffer-alist '("\\*helm" (display-buffer-in-side-window) (side . bottom) (window-height . 0.3)))

(use-package projectile
  :init (projectile-mode +1)
  :config (setq compile-command "make -j$(nproc)")
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :after (helm projectile)
  :config (helm-projectile-on)
  :bind (("C-c p s" . helm-projectile-rg) ("C-c p f" . helm-projectile-find-file)))

;; --- 6. LSP, Multi-Linter & Tree-Sitter ---
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (prog-mode . lsp-deferred)
  :config
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python"))
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  (add-to-list 'lsp-language-id-configuration '(vhdl-mode . "vhdl"))
  (setq lsp-clients-svlangserver-executable "/usr/local/bin/svlangserver" lsp-vhdl-server 'vhdl-ls)
  (setq lsp-diagnostics-provider :flycheck lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui :config (setq lsp-ui-doc-enable t lsp-ui-sideline-enable t))
(use-package lsp-treemacs :after (lsp treemacs) :config (lsp-treemacs-sync-mode 1))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-vhdl-ghdl-executable "ghdl" flycheck-verilog-verilator-executable "verilator")
  (when (executable-find "ruff") (setq flycheck-python-ruff-executable "ruff")))

;; --- 7. Org-Mode: Agenda, Capture, & Roam ---
(use-package org
  :bind (("C-c a" . org-agenda) ("C-c c" . org-capture))
  :config
  (setq org-directory "~/org"
        org-agenda-files '("~/org/agenda.org" "~/org/inbox.org")
        org-default-notes-file "~/org/inbox.org"
        org-log-done 'time)
  
  (setq org-capture-templates
        '(("t" "New Task" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n  Entered on: %U")
          ("l" "Engineering Log" entry (file+datetree "~/org/journal.org")
           "* %^{Log Title}\n  Time: %U\n\n  - Notes: %?"))))

(use-package org-roam
  :ensure t
  :custom (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-db-autosync-mode))

;; --- 8. Editing Tools (Smartparens, Ruff, Crux) ---
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config (require 'smartparens-config))

(use-package ruff-format
  :hook ((python-mode . ruff-format-on-save-mode) (python-ts-mode . ruff-format-on-save-mode)))

(use-package crux :bind (("C-a" . crux-move-beginning-of-line)))

(defun my/verilog-format-buffer ()
  (interactive)
  (when (and (eq major-mode 'verilog-mode) (executable-find "verible-verilog-format"))
    (call-process-region (point-min) (point-max) "verible-verilog-format" t t nil "-" "--inplace")))
(add-hook 'verilog-mode-hook (lambda () (add-hook 'before-save-hook #'my/verilog-format-buffer nil 'local)))

;; --- 9. Global Logic & Custom Keys ---
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "<f3>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f4>") (lambda () (interactive) (rainbow-delimiters-mode 'toggle)))
(global-set-key (kbd "<f5>") 'projectile-compile-project)
(use-package blamer :bind ("<f6>" . blamer-mode))

(defun my/hdl-toggle-source-header ()
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base (file-name-sans-extension (buffer-file-name)))
         (target (cond ((string= ext "v") "vh") ((string= ext "vh") "v")
                       ((string= ext "c") "h") ((string= ext "h") "c"))))
    (if (and target (file-exists-p (concat base "." target))) (find-file (concat base "." target)))))
(global-set-key (kbd "C-c t") 'my/hdl-toggle-source-header)

(defun my/indent-buffer () (interactive) (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c i") 'my/indent-buffer)

;; --- 10. Navigation & Completion ---
(use-package windsize :bind (("C-<up>" . windsize-up) ("C-<down>" . windsize-down) ("C-<left>" . windsize-left) ("C-<right>" . windsize-right)))
(use-package avy :bind ("M-s" . avy-goto-char-timer))
(use-package multiple-cursors :bind (("C->" . mc/mark-next-like-this) ("C-<" . mc/mark-previous-like-this)))
(use-package treemacs :bind ("C-x t t" . treemacs))
(use-package corfu :init (global-corfu-mode) :custom (corfu-auto t))
(use-package yasnippet :init (yas-global-mode 1))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; --- 11. Startup & Dashboard ---
(use-package dashboard :config (dashboard-setup-startup-hook))
(add-hook 'emacs-startup-hook (lambda () (treemacs) (switch-to-buffer "*dashboard*") (other-window 1)))

(setq gc-cons-threshold (* 2 1024 1024))
(provide 'init)
;;; init.el ends here
