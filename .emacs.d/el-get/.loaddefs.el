;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (auto-complete) "../../emacs/.emacs.d/el-get/auto-complete/auto-complete"
;;;;;;  "auto-complete/auto-complete.el" "8ed764024ac12abb7bc827ecf0897c7f")
;;; Generated autoloads from auto-complete/auto-complete.el

(autoload 'auto-complete "../../emacs/.emacs.d/el-get/auto-complete/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

;;;***

;;;### (autoloads (direx:jump-to-directory-other-window direx:jump-to-directory)
;;;;;;  "../../emacs/.emacs.d/el-get/direx/direx" "direx/direx.el"
;;;;;;  "092e378c65a4400c538e80a766be1ac1")
;;; Generated autoloads from direx/direx.el

(autoload 'direx:jump-to-directory "../../emacs/.emacs.d/el-get/direx/direx" "\


\(fn)" t nil)

(autoload 'direx:jump-to-directory-other-window "../../emacs/.emacs.d/el-get/direx/direx" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (direx-project:jump-to-project-root-other-window
;;;;;;  direx-project:jump-to-project-root) "../../emacs/.emacs.d/el-get/direx/direx-project"
;;;;;;  "direx/direx-project.el" "ccf52fecfa04e88c0f43d10fd96536d0")
;;; Generated autoloads from direx/direx-project.el

(autoload 'direx-project:jump-to-project-root "../../emacs/.emacs.d/el-get/direx/direx-project" "\


\(fn)" t nil)

(autoload 'direx-project:jump-to-project-root-other-window "../../emacs/.emacs.d/el-get/direx/direx-project" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (jedi:setup anything-jedi-related-names helm-jedi-related-names
;;;;;;  jedi:ac-setup jedi:complete jedi:start-dedicated-server)
;;;;;;  "../../emacs/.emacs.d/el-get/jedi/jedi" "jedi/jedi.el" "d744772d73291b5ae5f670a48dbe70c8")
;;; Generated autoloads from jedi/jedi.el

(autoload 'jedi:start-dedicated-server "../../emacs/.emacs.d/el-get/jedi/jedi" "\
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

(autoload 'jedi:complete "../../emacs/.emacs.d/el-get/jedi/jedi" "\
Complete code at point.

\(fn &key (expand ac-expand-on-auto-complete))" t nil)

(autoload 'jedi:ac-setup "../../emacs/.emacs.d/el-get/jedi/jedi" "\
Add Jedi AC sources to `ac-sources'.

If auto-completion is all you need, you can call this function instead
of `jedi:setup', like this::

   (add-hook 'python-mode-hook 'jedi:ac-setup)

Note that this function calls `auto-complete-mode' if it is not
already enabled, for people who don't call `global-auto-complete-mode'
in their Emacs configuration.

\(fn)" t nil)

(autoload 'helm-jedi-related-names "../../emacs/.emacs.d/el-get/jedi/jedi" "\
Find related names of the object at point using `helm' interface.

\(fn)" t nil)

(autoload 'anything-jedi-related-names "../../emacs/.emacs.d/el-get/jedi/jedi" "\
Find related names of the object at point using `anything' interface.

\(fn)" t nil)

(autoload 'jedi:setup "../../emacs/.emacs.d/el-get/jedi/jedi" "\
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

;;;### (autoloads (ecb-byte-compile ecb-minor-mode ecb-activate)
;;;;;;  "ecb/ecb" "../../../.emacs.d/el-get/ecb/ecb.el" "1891ea61e7c11a8a53ca678e04402279")
;;; Generated autoloads from ../../../.emacs.d/el-get/ecb/ecb.el

(autoload 'ecb-activate "ecb/ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument.

\(fn)" t nil)

(autoload 'ecb-minor-mode "ecb/ecb" "\
Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ecb-byte-compile "ecb/ecb" "\
Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist.

\(fn &optional FORCE-ALL)" t nil)

;;;***

;;;### (autoloads (ecb-show-help) "ecb/ecb-help" "../../../.emacs.d/el-get/ecb/ecb-help.el"
;;;;;;  "ad2944009b5fefed05b806382318ce03")
;;; Generated autoloads from ../../../.emacs.d/el-get/ecb/ecb-help.el

(autoload 'ecb-show-help "ecb/ecb-help" "\
Shows the online help of ECB in Info or HTML-format.
The format depends on the setting in `ecb-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help (Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `ecb-help-info-start-file' and `ecb-help-html-start-file'!

Note: If you got ECB as a standard XEmacs-package maybe the
HTML-online-documentation is not included.

\(fn &optional FORMAT)" t nil)

;;;***

;;;### (autoloads nil "ecb/ecb-util" "../../../.emacs.d/el-get/ecb/ecb-util.el"
;;;;;;  "ae8a1325a237da370204007b3ea5ebfc")
;;; Generated autoloads from ../../../.emacs.d/el-get/ecb/ecb-util.el

(defconst ecb-running-xemacs (featurep 'xemacs))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/el-get/ecb/ecb-advice-test.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-analyse.el" "../../../.emacs.d/el-get/ecb/ecb-autogen.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-autoloads.el" "../../../.emacs.d/el-get/ecb/ecb-buffertab.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-cedet-wrapper.el" "../../../.emacs.d/el-get/ecb/ecb-common-browser.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-compatibility.el" "../../../.emacs.d/el-get/ecb/ecb-compilation.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-create-layout.el" "../../../.emacs.d/el-get/ecb/ecb-cycle.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-eshell.el" "../../../.emacs.d/el-get/ecb/ecb-examples.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-face.el" "../../../.emacs.d/el-get/ecb/ecb-file-browser.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-help.el" "../../../.emacs.d/el-get/ecb/ecb-jde.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-layout-defs.el" "../../../.emacs.d/el-get/ecb/ecb-layout.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-method-browser.el" "../../../.emacs.d/el-get/ecb/ecb-mode-line.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-multiframe.el" "../../../.emacs.d/el-get/ecb/ecb-navigate.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-semantic-wrapper.el" "../../../.emacs.d/el-get/ecb/ecb-semantic.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-speedbar.el" "../../../.emacs.d/el-get/ecb/ecb-symboldef.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-tod.el" "../../../.emacs.d/el-get/ecb/ecb-upgrade.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb-util.el" "../../../.emacs.d/el-get/ecb/ecb-winman-support.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/ecb.el" "../../../.emacs.d/el-get/ecb/silentcomp.el"
;;;;;;  "../../../.emacs.d/el-get/ecb/tree-buffer.el" "auto-complete/auto-complete.el"
;;;;;;  "direx/direx-project.el" "direx/direx.el" "jedi/jedi.el")
;;;;;;  (21084 50224 669627))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
