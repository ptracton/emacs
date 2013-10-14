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

;;;### (autoloads (turn-on-bib-cite bib-cite-minor-mode) "bib-cite"
;;;;;;  "../../../../.emacs.d/el-get/auctex/bib-cite.el" (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/bib-cite.el

(autoload 'bib-cite-minor-mode "bib-cite" "\
Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display.

\(fn ARG)" t nil)

(autoload 'turn-on-bib-cite "bib-cite" "\
Unconditionally turn on Bib Cite mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads (context-mode) "context" "../../../../.emacs.d/el-get/auctex/context.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/context.el

(defalias 'ConTeXt-mode 'context-mode)

(autoload 'context-mode "context" "\
Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-en-mode) "context-en" "../../../../.emacs.d/el-get/auctex/context-en.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/context-en.el

(autoload 'context-en-mode "context-en" "\
Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

;;;***

;;;### (autoloads (context-nl-mode) "context-nl" "../../../../.emacs.d/el-get/auctex/context-nl.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/context-nl.el

(autoload 'context-nl-mode "context-nl" "\
Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook.

\(fn)" t nil)

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

;;;### (autoloads (ecb-byte-compile ecb-minor-mode ecb-activate)
;;;;;;  "ecb" "../../../../.emacs.d/el-get/ecb/ecb.el" (21084 30766))
;;; Generated autoloads from ../../../../.emacs.d/el-get/ecb/ecb.el

(autoload 'ecb-activate "ecb" "\
Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument.

\(fn)" t nil)

(autoload 'ecb-minor-mode "ecb" "\
Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'ecb-byte-compile "ecb" "\
Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist.

\(fn &optional FORCE-ALL)" t nil)

;;;***

;;;### (autoloads (ecb-show-help) "ecb-help" "../../../../.emacs.d/el-get/ecb/ecb-help.el"
;;;;;;  (21084 30766))
;;; Generated autoloads from ../../../../.emacs.d/el-get/ecb/ecb-help.el

(autoload 'ecb-show-help "ecb-help" "\
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

;;;### (autoloads nil "ecb-util" "../../../../.emacs.d/el-get/ecb/ecb-util.el"
;;;;;;  (21084 30766))
;;; Generated autoloads from ../../../../.emacs.d/el-get/ecb/ecb-util.el

(defconst ecb-running-xemacs (featurep 'xemacs))

;;;***

;;;### (autoloads (font-latex-setup) "font-latex" "../../../../.emacs.d/el-get/auctex/font-latex.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/font-latex.el

(autoload 'font-latex-setup "font-latex" "\
Setup this buffer for LaTeX font-lock.  Usually called from a hook.

\(fn)" nil nil)

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

;;;### (autoloads (docTeX-mode TeX-latex-mode BibTeX-auto-store)
;;;;;;  "latex" "../../../../.emacs.d/el-get/auctex/latex.el" (21084
;;;;;;  32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/latex.el

(autoload 'BibTeX-auto-store "latex" "\
This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

(autoload 'TeX-latex-mode "latex" "\
Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

(autoload 'docTeX-mode "latex" "\
Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

\(fn)" t nil)

(defalias 'TeX-doctex-mode 'docTeX-mode)

;;;***

;;;### (autoloads (multi-prompt-key-value multi-prompt) "multi-prompt"
;;;;;;  "../../../../.emacs.d/el-get/auctex/multi-prompt.el" (21084
;;;;;;  32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/multi-prompt.el

(autoload 'multi-prompt "multi-prompt" "\
Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

\(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil)

(autoload 'multi-prompt-key-value "multi-prompt" "\
Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

\(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

;;;***

;;;### (autoloads (ams-tex-mode TeX-plain-tex-mode) "plain-tex" "../../../../.emacs.d/el-get/auctex/plain-tex.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/plain-tex.el

(autoload 'TeX-plain-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook.

\(fn)" t nil)

(autoload 'ams-tex-mode "plain-tex" "\
Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (preview-report-bug LaTeX-preview-setup preview-install-styles)
;;;;;;  "preview" "../../../../.emacs.d/el-get/auctex/preview/preview.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/preview/preview.el

(autoload 'preview-install-styles "preview" "\
Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

\(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil)

(autoload 'LaTeX-preview-setup "preview" "\
Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality.

\(fn)" nil nil)
 (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(autoload 'preview-report-bug "preview" "\
Report a bug in the preview-latex package.

\(fn)" t nil)

;;;***

;;;### (autoloads (TeX-submit-bug-report TeX-auto-generate-global
;;;;;;  TeX-auto-generate TeX-tex-mode) "tex" "../../../../.emacs.d/el-get/auctex/tex.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex.el

(autoload 'TeX-tex-mode "tex" "\
Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'

\(fn)" t nil)

(autoload 'TeX-auto-generate "tex" "\
Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

\(fn TEX AUTO)" t nil)

(autoload 'TeX-auto-generate-global "tex" "\
Create global auto directory for global TeX macro definitions.

\(fn)" t nil)

(autoload 'TeX-submit-bug-report "tex" "\
Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration.

\(fn)" t nil)

;;;***

;;;### (autoloads (LaTeX-install-toolbar TeX-install-toolbar) "tex-bar"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-bar.el" (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex-bar.el

(autoload 'TeX-install-toolbar "tex-bar" "\
Install toolbar buttons for TeX mode.

\(fn)" t nil)

(autoload 'LaTeX-install-toolbar "tex-bar" "\
Install toolbar buttons for LaTeX mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tex-fold" "../../../../.emacs.d/el-get/auctex/tex-fold.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex-fold.el
 (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

(defalias 'tex-fold-mode 'TeX-fold-mode)

;;;***

;;;### (autoloads (tex-font-setup) "tex-font" "../../../../.emacs.d/el-get/auctex/tex-font.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex-font.el

(autoload 'tex-font-setup "tex-font" "\
Setup font lock support for TeX.

\(fn)" nil nil)

;;;***

;;;### (autoloads (TeX-texinfo-mode) "tex-info" "../../../../.emacs.d/el-get/auctex/tex-info.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex-info.el

(defalias 'Texinfo-mode 'texinfo-mode)

(autoload 'TeX-texinfo-mode "tex-info" "\
Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (japanese-latex-mode japanese-plain-tex-mode) "tex-jp"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-jp.el" (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/tex-jp.el

(autoload 'japanese-plain-tex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'.

\(fn)" t nil)

(autoload 'japanese-latex-mode "tex-jp" "\
Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (texmathp-match-switch texmathp) "texmathp" "../../../../.emacs.d/el-get/auctex/texmathp.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/texmathp.el

(autoload 'texmathp "texmathp" "\
Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked.

\(fn)" t nil)

(autoload 'texmathp-match-switch "texmathp" "\
Search backward for any of the math switches.
Limit searched to BOUND.

\(fn BOUND)" nil nil)

;;;***

;;;### (autoloads nil "toolbar-x" "../../../../.emacs.d/el-get/auctex/toolbar-x.el"
;;;;;;  (21084 32286))
;;; Generated autoloads from ../../../../.emacs.d/el-get/auctex/toolbar-x.el
 (autoload 'toolbarx-install-toolbar "toolbar-x")

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/el-get/auctex/auctex.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/auto-loads.el" "../../../../.emacs.d/el-get/auctex/bib-cite.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/context-en.el" "../../../../.emacs.d/el-get/auctex/context-nl.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/context.el" "../../../../.emacs.d/el-get/auctex/font-latex.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/latex.el" "../../../../.emacs.d/el-get/auctex/lpath.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/multi-prompt.el" "../../../../.emacs.d/el-get/auctex/plain-tex.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/preview/auto.el" "../../../../.emacs.d/el-get/auctex/preview/preview-latex.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/preview/preview.el" "../../../../.emacs.d/el-get/auctex/preview/prv-emacs.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/preview/prv-install.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/preview/prv-xemacs.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-bar.el" "../../../../.emacs.d/el-get/auctex/tex-buf.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-fold.el" "../../../../.emacs.d/el-get/auctex/tex-font.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-info.el" "../../../../.emacs.d/el-get/auctex/tex-jp.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-mik.el" "../../../../.emacs.d/el-get/auctex/tex-site.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex-style.el" "../../../../.emacs.d/el-get/auctex/tex-wizard.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/tex.el" "../../../../.emacs.d/el-get/auctex/texmathp.el"
;;;;;;  "../../../../.emacs.d/el-get/auctex/toolbar-x.el" "../../../../.emacs.d/el-get/cedet/cedet-build.el"
;;;;;;  "../../../../.emacs.d/el-get/cedet/cedet-devel-load.el" "../../../../.emacs.d/el-get/cedet/cedet-ediff.el"
;;;;;;  "../../../../.emacs.d/el-get/cedet/cedet-emacs-merge.el"
;;;;;;  "../../../../.emacs.d/el-get/cedet/cedet-remove-builtin.el"
;;;;;;  "../../../../.emacs.d/el-get/cedet/cedet-update-changelog.el"
;;;;;;  "../../../../.emacs.d/el-get/cedet/cedet-update-version.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-advice-test.el" "../../../../.emacs.d/el-get/ecb/ecb-analyse.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-autogen.el" "../../../../.emacs.d/el-get/ecb/ecb-autoloads.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-buffertab.el" "../../../../.emacs.d/el-get/ecb/ecb-cedet-wrapper.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-common-browser.el" "../../../../.emacs.d/el-get/ecb/ecb-compatibility.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-compilation.el" "../../../../.emacs.d/el-get/ecb/ecb-create-layout.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-cycle.el" "../../../../.emacs.d/el-get/ecb/ecb-eshell.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-examples.el" "../../../../.emacs.d/el-get/ecb/ecb-face.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-file-browser.el" "../../../../.emacs.d/el-get/ecb/ecb-help.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-jde.el" "../../../../.emacs.d/el-get/ecb/ecb-layout-defs.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-layout.el" "../../../../.emacs.d/el-get/ecb/ecb-method-browser.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-mode-line.el" "../../../../.emacs.d/el-get/ecb/ecb-multiframe.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-navigate.el" "../../../../.emacs.d/el-get/ecb/ecb-semantic-wrapper.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-semantic.el" "../../../../.emacs.d/el-get/ecb/ecb-speedbar.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-symboldef.el" "../../../../.emacs.d/el-get/ecb/ecb-tod.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-upgrade.el" "../../../../.emacs.d/el-get/ecb/ecb-util.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/ecb-winman-support.el" "../../../../.emacs.d/el-get/ecb/ecb.el"
;;;;;;  "../../../../.emacs.d/el-get/ecb/silentcomp.el" "../../../../.emacs.d/el-get/ecb/tree-buffer.el"
;;;;;;  "auto-complete/auto-complete-config.el" "auto-complete/auto-complete-pkg.el"
;;;;;;  "ctable/ctable.el" "ctable/test-ctable.el" "deferred/concurrent-sample.el"
;;;;;;  "deferred/concurrent.el" "deferred/deferred-samples.el" "deferred/deferred.el"
;;;;;;  "deferred/test-concurrent.el" "deferred/test-deferred.el"
;;;;;;  "epc/epc.el" "epc/epcs.el" "epc/test-epc.el" "fuzzy/fuzzy.el"
;;;;;;  "jedi/jedi-pkg.el" "jedi/test-jedi.el" "jedi/tryout-jedi.el"
;;;;;;  "popup/popup.el") (21084 32295 756485))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
