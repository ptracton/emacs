
;;
;; http://doc.norang.ca/org-mode.html#GettingOrgModeWithGit
;;

;;;
;;; Org Mode
;;;
(require 'org)
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)

;(add-to-list 'load-path (expand-file-name "~/org-mode/org-8.3.2/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))


					  
;(setq org-latex-listings nil)

(setq org-agenda-files '("~/Dropbox/Agenda/"))
(setq org-ditaa-jar-path "/usr/bin/ditaa")
(setq org-plantuml-jar-path "/home/ptracton/org-mode/plantuml.jar")

;(org-babel-do-load-languages
; (quote org-babel-load-languages)
; (quote ((emacs-lisp . t)
;         (dot . t)
;         (ditaa . t)
;         (R . t)
;         (python . t)
;         (ruby . t)
;         (gnuplot . t)
;         (clojure . t)
;         (sh . t)
;         (ledger . t)
;         (org . t)
;         (plantuml . t)
;         (latex . t))))


;;
;; Standard key bindings
;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-e" 'end-of-line); overwrites org-mode \C-e definition
(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)
(global-set-key (kbd "<f12>") 'org-agenda)

;; I like to press enter to follow a link. mouse clicks also work.
(setq org-return-follows-link t)

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;; use this code in emacs-lisp for folding code.
(global-set-key (kbd "C-M-]") (lambda () (interactive) (org-cycle t)))
(global-set-key (kbd "M-]") (lambda ()
                              (interactive)
                              (ignore-errors
                                (end-of-defun)
                                (beginning-of-defun))
                              (org-cycle)))

(setq org-completion-use-ido t)

;; I do not like this mode
(auto-fill-mode -1)

;; turn off auto-fill in org-mode. It is not enough to turn it off
;; everywhere.
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

;; capture key binding
(define-key global-map "\C-cc" 'org-capture)

;; setup archive location in archive directory in current folder
(setq org-archive-location "archive/%s_archive::")


;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

;; language specific headers. I think this comes before the defaults
;; for emacs-lisp I want results to be value
(setq org-babel-default-header-args:emacs-lisp
      (cons '(:results . "value replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; for everything else set default :results to output
(setq org-babel-default-header-args
      (cons '(:results . "output replace")
	    (assq-delete-all :results org-babel-default-header-args)))

;; set default exports to both code and results
(setq org-babel-default-header-args
      (cons '(:exports . "both")
	    (assq-delete-all :exports org-babel-default-header-args)))

;; Interpret "_" and "^" for export when braces are used.
(setq org-export-with-sub-superscripts '{})


;;* Agenda setup
; I don't want to see things that are done. turn that off here.
; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-timestamp t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-start-on-weekday nil) ;; start on current day

(setq org-upcoming-deadline '(:foreground "blue" :weight bold))

;; give me some warning of upcoming deadlines
(setq org-deadline-warning-days 0)

;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

;;* Export settings
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)
	("" "url" nil)
	("" "minted" nil)
;	("" "underscore" nil)
	("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	 "hyperref" nil)
	("" "attachfile" nil)))


;; this is for code syntax highlighting in export
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
 (setq org-latex-pdf-process
       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
         "bibtex %b"
         "makeindex %b"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b"))


;; make it possible to insert from helm.
(defun helm-insert-org-figure (target)
  (jmax-insert-figure
   (file-relative-name target)
   (read-input "Width: ")
   (read-input "Parameters: ")))


(add-hook 'helm-find-files-before-init-hook
          (lambda ()
	    (helm-add-action-to-source
	     "Insert as org-mode figure"
	     'helm-insert-org-figure
	     helm-source-find-files)))


(defun jmax-insert-table (ncols tblname attributes)
  "Insert a table with NCOLS and named TBLNAME.
If you enter ATTRIBUTES they are inserted as LaTeX attributes."
  (interactive "nColumns: \nsName: \nsAttributes: ")
  (when (not (string= "" tblname))
    (insert (format "#+tblname: %s\n" tblname)))
  (when (not (string= "" attributes))
    (insert (format "#+attr_latex: %s\n" attributes)))
  (insert "#+caption: \n")
  (loop
   initially (insert "|")
   repeat ncols do (insert "  |" )
   finally (insert "\n"))
  (forward-line -2)
  (end-of-line))


;;** Asynchronous Python
(defun org-babel-async-execute:python ()
  "Execute the python src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.

A new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block."
  (interactive)
  (let* ((current-file (buffer-file-name))
	 (uuid (org-id-uuid))
	 (code (org-element-property :value (org-element-context)))
	 (temporary-file-directory ".")
	 (tempfile (make-temp-file "py-"))
	 (pbuffer (format "*%s*" uuid))
	 (varcmds (org-babel-variable-assignments:python
		   (nth 2 (org-babel-get-src-block-info))))
	 process)

    ;; get rid of old results, and put a place-holder for the new results to
    ;; come.
    (org-babel-remove-result)

    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format
	       "\n\n#+RESULTS: %s\n: %s"
	       (or (org-element-property :name (org-element-context))
		   "")
	       uuid)))

    ;; open the results buffer to see the results in.
    (switch-to-buffer-other-window pbuffer)

    ;; Create temp file containing the code.
    (with-temp-file tempfile
      ;; if there are :var headers insert them.
      (dolist (cmd varcmds)
	(insert cmd)
	(insert "\n"))
      (insert code))

    ;; run the code
    (setq process (start-process
		   uuid
		   pbuffer
		   "python"
		   tempfile))
    
    ;; when the process is done, run this code to put the results in the
    ;; org-mode buffer.
    (set-process-sentinel
     process
     `(lambda (process event)
	(delete-file ,tempfile)
	(unwind-protect
	    (save-window-excursion
	      (save-excursion
		(save-restriction
		  (with-current-buffer (find-file-noselect ,current-file)
		    (goto-char (point-min))
		    (when (re-search-forward ,uuid nil t)
		      (beginning-of-line)
		      (kill-line)
		      (when (with-current-buffer
				,pbuffer
			      (buffer-string)))
		      (insert
		       (mapconcat
			(lambda (x)
			  (format ": %s" x))
			(butlast (split-string
				  (with-current-buffer
				      ,pbuffer
				    (buffer-string))
				  "\n"))
			"\n")))))))
	  ;; delete the results buffer then delete the tempfile.
	  ;; finally, delete the process.
	  (when (get-buffer ,pbuffer)
	    (kill-buffer ,pbuffer)
	    (delete-window)) 
	  (delete-process process))))))


(defun org-babel-kill-async ()
  "Kill the current async process.
Run this in the code block that is running."
  (interactive)
  (goto-char (org-babel-where-is-src-block-result))
  (forward-line)
  (forward-char)
  (interrupt-process
   (s-trim (buffer-substring (point) (line-end-position)))))


(defun jmax-todo-agenda ()
  "Show the todo agenda in the current directory."
  (interactive)
  (let ((org-agenda-files (f-files "." (lambda (f) (f-ext? f "org")))))
    (org-agenda nil "t")))


;; * Kitchinhub weekly report

(defun kitchinhub-weekly-report ()
  "Create and open the report due next Tuesday." 
  (let* ((report-dir (concat "reports/" (org-read-date nil nil "Tue")))
	 (status (unless (file-directory-p report-dir)
		   (make-directory report-dir t)))
	 (default-directory (expand-file-name (file-name-as-directory report-dir))))
    (ox-manuscript-new-manuscript "weekly-progress-report")))

(defun kitchinhub-annual-review ()
  "Create and open the annual review."
  (let* ((review-dir (format "reports/annual-review-%s"
			     (calendar-extract-year (calendar-current-date))))
	 (status (unless (file-directory-p review-dir)
		   (make-directory review-dir t)))
	 (default-directory (expand-file-name (file-name-as-directory review-dir))))
    (ox-manuscript-new-manuscript "annual-student-review")))


;;
;; Use evince to view the PDF files
;;
(eval-after-load "org"
  '(progn
     ;; .txt files aren't in the list initially, but in case that changes
     ;; in a future version of org, use if to avoid errors
     (if (assoc "\\.txt\\'" org-file-apps)
         (setcdr (assoc "\\.txt\\'" org-file-apps) "notepad.exe %s")
       (add-to-list 'org-file-apps '("\\.txt\\'" . "notepad.exe %s") t))
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

;;
;; http://orgmode.org/manual/Code-evaluation-security.html
;;
(setq org-agenda-include-diary t)

;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)


;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

;; this is for code syntax highlighting in export
;(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))

;; avoid getting \maketitle right after begin{document}
;; you should put \maketitle if and where you want it.
(setq org-latex-title-command "")

