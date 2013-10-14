((auctex status "installed" recipe
	 (:name auctex :website "http://www.gnu.org/software/auctex/" :description "AUCTeX is an extensible package for writing and formatting TeX files in GNU Emacs and XEmacs. It supports many different TeX macro packages, including AMS-TeX, LaTeX, Texinfo, ConTeXt, and docTeX (dtx files)." :type git :module "auctex" :url "git://git.savannah.gnu.org/auctex.git" :build
		`(("./autogen.sh")
		  ("./configure" "--without-texmf-dir" ,(cond
							 ((eq system-type 'darwin)
							  "--with-lispdir=`pwd`")
							 (t ""))
		   ,(concat "--with-emacs=" el-get-emacs))
		  "make")
		:load-path
		("." "preview")
		:load
		("tex-site.el" "preview/preview-latex.el")
		:info "doc"))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)))
 (cedet status "installed" recipe
	(:name cedet :website "http://cedet.sourceforge.net/" :description "CEDET is a Collection of Emacs Development Environment Tools written with the end goal of creating an advanced development environment in Emacs." :type bzr :url "bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk" :build
	       `(("sh" "-c" "touch `find . -name Makefile`")
		 ("make" ,(format "EMACS=%s"
				  (shell-quote-argument el-get-emacs))
		  "clean-all")
		 ("make" ,(format "EMACS=%s"
				  (shell-quote-argument el-get-emacs)))
		 ("make" ,(format "EMACS=%s"
				  (shell-quote-argument el-get-emacs))
		  "-C" "contrib"))
	       :build/berkeley-unix
	       `(("sh" "-c" "touch `find . -name Makefile`")
		 ("gmake" ,(format "EMACS=%s"
				   (shell-quote-argument el-get-emacs))
		  "clean-all")
		 ("gmake" ,(format "EMACS=%s"
				   (shell-quote-argument el-get-emacs)))
		 ("gmake" ,(format "EMACS=%s"
				   (shell-quote-argument el-get-emacs))
		  "-C" "contrib"))
	       :build/windows-nt
	       ("echo #!/bin/sh > tmp.sh & echo touch `/usr/bin/find . -name Makefile` >> tmp.sh & echo make FIND=/usr/bin/find >> tmp.sh" "sed 's/^M$//' tmp.sh  > tmp2.sh" "sh ./tmp2.sh" "rm ./tmp.sh ./tmp2.sh")
	       :features nil :lazy nil :post-init
	       (unless
		   (featurep 'cedet-devel-load)
		 (load
		  (expand-file-name "cedet-devel-load.el" pdir)))))
 (ctable status "installed" recipe
	 (:name ctable :description "Table Component for elisp" :type github :pkgname "kiwanami/emacs-ctable"))
 (deferred status "installed" recipe
   (:name deferred :description "Simple asynchronous functions for emacs lisp" :website "https://github.com/kiwanami/emacs-deferred" :type github :pkgname "kiwanami/emacs-deferred" :features "deferred"))
 (direx status "installed" recipe
	(:name direx :description "Directory Explorer" :website "https://github.com/m2ym/direx-el" :type github :pkgname "m2ym/direx-el" :features "direx"))
 (ecb status "installed" recipe
      (:name ecb :description "Emacs Code Browser" :type github :pkgname "alexott/ecb" :depends cedet :build
	     `(("make" "CEDET=../cedet" ,(concat "EMACS="
						 (shell-quote-argument el-get-emacs))))))
 (epc status "installed" recipe
      (:name epc :description "An RPC stack for Emacs Lisp" :type github :pkgname "kiwanami/emacs-epc" :depends
	     (deferred ctable)))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (jedi status "installed" recipe
       (:name jedi :description "An awesome Python auto-completion for Emacs" :type github :pkgname "tkf/emacs-jedi" :build
	      (("make" "requirements"))
	      :build/windows-nt
	      (("make" "requirements" "PYTHON=python.exe" "BINDIR=Scripts"))
	      :build/berkeley-unix
	      (("gmake" "requirements"))
	      :submodule nil :depends
	      (epc auto-complete)))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :pkgname "auto-complete/popup-el")))
