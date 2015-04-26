
(semantic-mode 1)
(global-ede-mode 1)                      ; Enable the Project management system

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

