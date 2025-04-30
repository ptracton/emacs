;;; my-embedded.el --- Embedded & FPGA development helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds useful tools and keybindings for embedded systems and HDL development

;;; Code:

;; HEX editor (built-in)
(use-package hexl
  :ensure nil) ;; built-in

;; Serial monitor (for microcontrollers)
(defun open-serial-monitor ()
  "Open serial monitor on /dev/ttyUSB0 at 115200 baud."
  (interactive)
  (serial-term "/dev/ttyUSB0" 115200))

;; VCD waveform viewer
(defun open-gtkwave ()
  "Open the most recent VCD file in gtkwave."
  (interactive)
  (let ((vcd (car (last (file-expand-wildcards "*.vcd")))))
    (if vcd
        (start-process "gtkwave" nil "gtkwave" vcd)
      (message "No .vcd file found."))))

;; Build shortcut
(global-set-key (kbd "<f12>") 'compile)
(setq compile-command "make -k")

;; VCD view shortcut
(global-set-key (kbd "S-<f12>") 'open-gtkwave)

;; CMake for embedded C projects
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; TCL scripting (Vivado, Quartus)
(add-to-list 'auto-mode-alist '("\\.tcl\\'" . tcl-mode))

;; Org integration (optional for project notes)
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("tcl" . tcl)))

(provide 'my-embedded)
;;; my-embedded.el ends here
