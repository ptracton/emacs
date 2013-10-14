(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(row-number-mode t)
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;;
;;; Manually edited
;;;

;;
;; Get rid of annoying startup message
;;
(setq inhibit-startup-message t)        ; Do without annoying startup msg.


;;
;; Add paths to 3rd party tools
;;
(add-to-list 'load-path "/home/ptracton/.emacs.d/third-party/")
(add-to-list 'load-path "/home/ptracton/.emacs.d/third-party/color-theme-6.6.0")
(add-to-list 'load-path "/home/ptracton/.emacs.d/third-party/cedet-1.1/common")
(add-to-list 'load-path "/home/ptracton/.emacs.d/third-party/el-get")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/jedi")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/auto-complete")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/ctable")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/deferred")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/direx")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/epc")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/fuzzy")
(add-to-list 'load-path "/home/ptracton/.emacs.d/el-get/popup")

;;
;; CEDET
;;

(load-file "/home/ptracton/.emacs.d/third-party/cedet-1.1/common/cedet.el")

(setq semantic-default-submodes
      '(;; cache(?)
        global-semanticdb-minor-mode

        global-semantic-highlight-edits-mode
        global-semantic-idle-local-symbol-highlight-mode
        ;; global-cedet-m3-minor-mode

        ;; code helpers
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode

        ;; eye candy
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-highlight-edits-mode
        global-semantic-stickyfunc-mode

        ;; debugging semantic itself
        ;;global-semantic-show-parser-state-mode 1   ;; show the parsing state in the mode line
        ;;global-semantic-show-unmatched-syntax-mode 1
        ))

(global-ede-mode 1)
(speedbar 1)

;; Enable EDE (Project Management) features
;;(global-ede-mode 1)

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;;(semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

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
;(require 'direx)
;(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;;
;; Jedi
;; http://tkf.github.io/emacs-jedi/released/
;;
(load-file "/home/ptracton/.emacs.d/el-get/jedi/jedi.el")
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

;;
;; Jedi-direx
;; https://github.com/tkf/emacs-jedi-direx
;;
;(eval-after-load "python"
;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))


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


(setq ‘next-line-add-newlines’ 't)

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
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

