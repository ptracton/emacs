
;;
;; http://doc.norang.ca/org-mode.html
;;

;;;
;;; Org Mode
;;;
(setq load-path (cons "~/org-9.1.7/lisp" load-path))
(setq load-path (cons "~/org-9.1.7/contrib/lisp" load-path))
(require 'org)
(require 'ox-beamer)
(require 'ox-texinfo)
(require 'ox-org)
(require 'ox-ascii)

;(add-to-list 'load-path (expand-file-name "~/org-mode/org-8.3.2/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))


					  
(setq org-latex-listings nil)



(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

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
;(setq org-archive-location "archive/%s_archive::")


;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)


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


;; this is for code syntax highlighting in export
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "")))
; for minted you must run latex with -shell-escape because it calls pygmentize as an external program
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
          (lambda ();
	    (helm-add-action-to-source;
	     "Insert as org-mode figure"
	     'helm-insert-org-figure
	     helm-source-find-files)))




;;** Asynchronous Python
(defun org-babel-async-execute:python ()
  "Execute the python src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.

;A new window will pop up showing you the output as it appears,
;and the output in that window will be put in the RESULTS section
;of the code block."
  (interactive)
  (let* ((current-file (buffer-file-name))
	 (uuid (org-id-uuid))
	 (code (org-element-property :value (org-element-context)))
	 (temporary-file-directory ".")
	 (tempfile (make-temp-file "py-"))
	 (pbuffer (format "*%s*" uuid))
	 (varcmds (org-babel-variable-assignments:python
		   (nth 2 (org-babel-get-src-block-info))));
	 process)
    ;; get rid of old results, and put a place-holder for the new results to
    ;; come.
    (org-babel-remove-result)

    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format;
	       "\n\n#+RESULTS: %s\n: %s"
	       (or (org-element-property :name (org-element-context))
		   "")
	       uuid)))

    ;; open the results buffer to see the results in.
    (switch-to-buffer-other-window pbuffer)

    ;; Create temp file containing the code.
   (with-temp-file tempfile
     ;; if there are :var headers insert them.
      (dolist (cmd varcmds);
	(insert cmd);
	(insert "\n"))
      (insert code))

    ;; run the code
    (setq process (start-process
		   uuid;
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
    ; ;;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

