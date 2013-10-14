(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(row-number-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;;
;;; Manually edited
;;;

;;
;; Get rid of annoying startup message
;;
(setq inhibit-startup-message t)        


;;
;; Add paths to 3rd party tools
;;
(add-to-list 'load-path "~/.emacs.d/third-party/")
(add-to-list 'load-path "~/.emacs.d/third-party/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/third-party/el-get")
(add-to-list 'load-path "~/.emacs.d/el-get/cedet")
(add-to-list 'load-path "~/.emacs.d/el-get/jedi")
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete")
(add-to-list 'load-path "~/.emacs.d/el-get/ctable")
(add-to-list 'load-path "~/.emacs.d/el-get/deferred")
(add-to-list 'load-path "~/.emacs.d/el-get/direx")
(add-to-list 'load-path "~/.emacs.d/el-get/epc")
(add-to-list 'load-path "~/.emacs.d/el-get/fuzzy")
(add-to-list 'load-path "~/.emacs.d/el-get/popup")
(add-to-list 'load-path "~/.emacs.d/el-get/ecb")
(add-to-list 'load-path "~/.emacs.d/el-get/auctex")

;;
;; CEDET
;;
(load-file "~/.emacs.d/el-get/cedet/cedet-devel-load.el")

(require 'ecb)

(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc) ; or depending on you compiler

(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


;;
;; el-get
;; https://github.com/dimitri/el-get
;;
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/third-party/el-get-user/recipes")
(el-get 'sync)

;;
;; direx-el
;; https://github.com/m2ym/direx-el
;;
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;;
;; Jedi
;; http://tkf.github.io/emacs-jedi/released/
;;
(load-file "~/.emacs.d/el-get/jedi/jedi.el")
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;;
;; Jedi-direx
;; https://github.com/tkf/emacs-jedi-direx
;;
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))


;;
;; Put the scroll bar on the right side
;; http://www.emacswiki.org/emacs/ScrollBar
;;
(set-scroll-bar-mode 'right) 

;;
;; Color Themes
;;
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)
(setq my-color-themes (list 'color-theme-arjen 'color-theme-clarity
                              'color-theme-hober 'color-theme-billw
                              'color-theme-lethe 'color-theme-ld-dark
                              'color-theme-charcoal-black 'color-theme-late-night
                              'color-theme-midnight 'color-theme-tty-dark))
  (defun my-theme-set-default () ; Set the first row
      (interactive)
      (setq theme-current my-color-themes)
      (funcall (car theme-current)))
     
    (defun my-describe-theme () ; Show the current theme
      (interactive)
      (message "%s" (car theme-current)))
     
   ; Set the next theme (fixed by Chris Webber - tanks)
    (defun my-theme-cycle ()            
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
      (setq theme-current my-color-themes))
      (funcall (car theme-current))
      (message "%S" (car theme-current)))
    
    (setq theme-current my-color-themes)
    (setq color-theme-is-global nil) ; Initialization
    (my-theme-set-default)
    (global-set-key [f12] 'my-theme-cycle)

;;
;; http://www.emacswiki.org/emacs/NoTabs
;;
(setq indent-tabs-mode nil) 

;;
;; http://www.emacswiki.org/emacs/CProgrammingLanguage
;;
(setq c-default-style "python")
(setq-default c-basic-offset 4)
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;;
;; Recentf is a minor mode that builds a list of recently opened files. This list is is automatically saved across Emacs sessions. You can then access this list through a menu.
;; http://www.emacswiki.org/emacs/RecentFiles
;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;
;; http://www.emacswiki.org/emacs/FrameTitle
;;
 (setq frame-title-format
          '(buffer-file-name
            "%f"
            (dired-directory dired-directory "%b")))

;;
;; Verilog mode customization
;; http://www.cs.washington.edu/education/courses/cse467/04wi/misc/verilog-mode.el
;;
(setq verilog-indent-level                 4
          verilog-indent-level-module      4
          verilog-indent-level-declaration 4
          verilog-indent-level-behavioral  4
          verilog-indent-level-directive   4
          verilog-case-indent              2
          verilog-auto-newline             t
          verilog-auto-indent-on-newline   t
          verilog-auto-endcomments         t
	  verilog-minimum-comment-distance 40
          verilog-indent-begin-after-if    t
          verilog-auto-lineup              '(all))

;;
;; http://www.veripool.org/projects/verilog-mode/wiki/Faq#Why-when-others-edit-my-code-does-it-looks-unindented
;;
(add-hook 'verilog-mode-hook '(lambda ()
    (add-hook 'local-write-file-hooks (lambda()
       (untabify (point-min) (point-max))))))

(add-hook 'verilog-mode-hook '(lambda ()
  (add-hook 'write-file-functions (lambda()
      (untabify (point-min) (point-max))
      nil))))

;;
;; http://www.emacswiki.org/emacs/AUCTeX
;;
;; (setq TeX-auto-save t)
;;     (setq TeX-parse-self t)
;;     (setq-default TeX-master nil)
;;     (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;     (setq reftex-plug-into-AUCTeX t)
;; (setq TeX-PDF-mode t)
;; (setq reftex-plug-into-AUCTeX t)


;;
;; Force .c and .h files into C++ mode since C mode does not handle doxygen 
;; comments correctly!
;;
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
