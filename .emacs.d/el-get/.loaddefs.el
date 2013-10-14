;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (auto-complete) "auto-complete" "auto-complete/auto-complete.el"
;;;;;;  (21084 8813))
;;; Generated autoloads from auto-complete/auto-complete.el

(autoload 'auto-complete "auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

;;;***

;;;### (autoloads (direx:jump-to-directory-other-window direx:jump-to-directory)
;;;;;;  "direx" "direx/direx.el" (21084 10470))
;;; Generated autoloads from direx/direx.el

(autoload 'direx:jump-to-directory "direx" "\
Not documented

\(fn)" t nil)

(autoload 'direx:jump-to-directory-other-window "direx" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (direx-project:jump-to-project-root-other-window
;;;;;;  direx-project:jump-to-project-root) "direx-project" "direx/direx-project.el"
;;;;;;  (21084 10470))
;;; Generated autoloads from direx/direx-project.el

(autoload 'direx-project:jump-to-project-root "direx-project" "\
Not documented

\(fn)" t nil)

(autoload 'direx-project:jump-to-project-root-other-window "direx-project" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (jedi:setup anything-jedi-related-names helm-jedi-related-names
;;;;;;  jedi:ac-setup jedi:complete jedi:start-dedicated-server)
;;;;;;  "jedi" "jedi/jedi.el" (21084 9447))
;;; Generated autoloads from jedi/jedi.el

(autoload 'jedi:start-dedicated-server "jedi" "\
Start Jedi server dedicated to this buffer.
This is useful, for example, when you want to use different
`sys.path' for some buffer.  When invoked as an interactive
command, it asks you how to start the Jedi server.  You can edit
the command in minibuffer to specify the way Jedi server run.

If you want to setup how Jedi server is started programmatically
per-buffer/per-project basis, make `jedi:server-command' and
`jedi:server-args' buffer local and set it in `python-mode-hook'.
See also: `jedi:server-args'.

\(fn COMMAND)" t nil)

(autoload 'jedi:complete "jedi" "\
Complete code at point.

\(fn &key (EXPAND ac-expand-on-auto-complete))" t nil)

(autoload 'jedi:ac-setup "jedi" "\
Add Jedi AC sources to `ac-sources'.

If auto-completion is all you need, you can call this function instead
of `jedi:setup', like this::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

Note that this function calls `auto-complete-mode' if it is not
already enabled, for people who don't call `global-auto-complete-mode'
in their Emacs configuration.

\(fn)" t nil)

(autoload 'helm-jedi-related-names "jedi" "\
Find related names of the object at point using `helm' interface.

\(fn)" t nil)

(autoload 'anything-jedi-related-names "jedi" "\
Find related names of the object at point using `anything' interface.

\(fn)" t nil)

(autoload 'jedi:setup "jedi" "\
Fully setup jedi.el for current buffer.
It setups `ac-sources' (calls `jedi:ac-setup') and turns
`jedi-mode' on.

This function is intended to be called from `python-mode-hook',
like this::

       (add-hook 'python-mode-hook 'jedi:setup)

You can also call this function as a command, to quickly test
what jedi can do.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete/auto-complete-config.el"
;;;;;;  "auto-complete/auto-complete-pkg.el" "ctable/ctable.el" "ctable/test-ctable.el"
;;;;;;  "deferred/concurrent-sample.el" "deferred/concurrent.el"
;;;;;;  "deferred/deferred-samples.el" "deferred/deferred.el" "deferred/test-concurrent.el"
;;;;;;  "deferred/test-deferred.el" "epc/epc.el" "epc/epcs.el" "epc/test-epc.el"
;;;;;;  "fuzzy/fuzzy.el" "jedi/jedi-pkg.el" "jedi/test-jedi.el" "jedi/tryout-jedi.el"
;;;;;;  "popup/popup.el") (21084 10471 265421))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
