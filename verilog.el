(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(load-file "~/.emacs.d/custom/auto-complete-verilog.el")
;(require 'auto-complete-verilog.el)
