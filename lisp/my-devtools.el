;;; my-devtools.el --- Developer power tools for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Extra helpers for embedded, HDL, and systems programming workflows.
;; Includes multiple cursors, ripgrep, symbol outline, dumb-jump fallback, and more.

;;; Code:

;; Smart project search
(use-package ripgrep)

;; Dumb-jump (fallback when LSP isn't working)
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Multiple cursors (VSCode-like selection)
(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; LSP Outline View
(use-package lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1))

;; Offline docs (requires DevDocs app or Internet)
(use-package devdocs
  :bind (("C-c d d" . devdocs-lookup)))

;; Project-specific environments (useful for FPGA SDKs)
(use-package envrc
  :hook (prog-mode . envrc-mode))

;; Git Timemachine (view file history per line)
(use-package git-timemachine
  :bind (("C-x t m" . git-timemachine)))

;; Highlight TODO / FIXME across projects
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FFB86C")
          ("FIXME"  . "#FF6C6B")
          ("HACK"   . "#BD93F9")
          ("REVIEW" . "#8BE9FD")
          ("NOTE"   . "#50FA7B"))))

;; Terminal emulator (better than ansi-term)
(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash")) ;; or zsh/fish depending on your setup

(use-package blamer
  :config (global-blamer-mode 1))


(provide 'my-devtools)
;;; my-devtools.el ends here
