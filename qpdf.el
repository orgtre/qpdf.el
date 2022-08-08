;;; qpdf.el --- A transient Emacs wrapper for qpdf -*- lexical-binding: t; -*-

;;; Commentary:

;; This requires that qpdf is installed and in your path.
;; Linux/Windows: https://github.com/qpdf/qpdf/releases/
;; MacOS: https://formulae.brew.sh/formula/qpdf

(require 'transient)


(defgroup qpdf.el nil
  "A transient Emacs wrapper for qpdf."
  :group 'multimedia)

(defcustom qpdf-docs-url "https://qpdf.readthedocs.io/en/stable/cli.html"
  "The url used by `qpdf-docs'."
  :group 'qpdf.el
  :type 'string)

(defcustom qpdf-pages-prepromt
  "Synatax: 'file [--password=password] [page-range] [...] --'.
If only one file, 'file' and ' --' can also be omitted.
Example page-ranges: '1,6-10,4,2,30-20,r3-z', '1-9:even', '1,4,5:odd'.\n"
  "Extra syntax explanations shown before the --pages custom prompt."
  :group 'qpdf.el
  :type 'string)

(defcustom qpdf-custom-prepromt
  "List any options exactly as one would in the command line.\n"
  "Extra syntax explanations shown before the --custom prompt."
  :group 'qpdf.el
  :type 'string)

(defcustom qpdf-set-defaults-function 'qpdf--default-set-defaults-function
  "Function used to set the defaults of `qpdf'.
Should take no arguments and return a list of key-value strings."
  :group 'qpdf.el
  :type 'symbol)

(defcustom qpdf-read-pages-function 'qpdf--default-read-pages-function
  "Function used to read the `--pages=' argument.
Should take three arguments: prompt initial-input history and output a string."
  :group 'qpdf.el
  :type 'symbol)

(defcustom qpdf-run-after-functions '(qpdf--default-run-after-function)
  "List of functions to call after the qpdf shell command has been run.
Each should take one argument, the transient-args passed down from `qpdf'."
  :group 'qpdf.el
  :type 'list)

(defcustom qpdf-default-outfile "qpdf-outfile.pdf"
  "Name for the default outfile."
  :group 'qpdf.el
  :type 'string)


;;;###autoload
(transient-define-prefix qpdf ()
  "Transient dispatcher for the qpdf shell command.
See URL `https://qpdf.readthedocs.io/en/stable/cli.html#page-selection'
for details on the --pages argument and others."
  :init-value 'qpdf--set-defaults
  :transient-non-suffix 'transient--do-stay
  :incompatible '(("--replace-input" "--outfile="))
  ["Arguments"
   ("p" "pages" "--pages=" qpdf--read-pages)
   ("i" "infile" "--infile=" qpdf--read-file)
   ("o" "outfile" "--outfile=" qpdf--read-file)
   ("r" "replace input" "--replace-input")
   (qpdf--flatten-annotations)
   ("c" "custom" "--custom=" qpdf--read-custom)]
  [["Actions"
    ("<return>" " qpdf-run" qpdf-run)]
   [""
    ("h" "qpdf-docs"
     qpdf-docs :transient t)]])


;;;###autoload
(defun qpdf--set-defaults (obj)
  "Set dynamic initial values for `qpdf'."
  (oset obj value (funcall qpdf-set-defaults-function)))


(defun qpdf--default-set-defaults-function ()
  `(,(if (equal major-mode 'pdf-view-mode)
	 (concat "--pages="				 
		 (concat ". " (number-to-string
			       (image-mode-window-get 'page))
			 " --")
		 nil))
    ,(concat "--infile="
	     (if (equal major-mode 'pdf-view-mode)
		 (pdf-view-buffer-file-name)
	       "--empty"))
    ,(concat "--outfile="
	     (qpdf--make-unique-filename
	      (file-truename qpdf-default-outfile)))))
  

(transient-define-argument qpdf--flatten-annotations ()
  "Set up the --flatten-annotations argument as a switch."
  :description "flatten"
  :class 'transient-switches
  :key "f"
  :argument-format "--flatten-annotations=%s"
  :argument-regexp "\\(screen\\|print\\|all\\)"
  :choices '("screen" "print" "all"))


(defun qpdf-run (&optional args)
  "Run shell command qpdf.
If called interactively `ARGS' are taken from `transient-current-command`, which
typically is `qpdf'. If called non-interactively `ARGS' should be a list of
strings containing qpdf command-line options and their values. See URL
`https://qpdf.readthedocs.io/en/stable/cli.html'. The strings
should contain exactly what one would enter in the command line, e.g.
'--flatten-annotations=screen'. Exceptions are strings starting with
'--pages=', '--infile=', '--outfile=', and '--custom=', which are modified to
fit the qpdf signature."
  (interactive (list (transient-args transient-current-command)))
  (print args)
  (let ((pages (transient-arg-value "--pages=" args))
	(infile (transient-arg-value "--infile=" args))
	(outfile (transient-arg-value "--outfile=" args))
	(replace-input (transient-arg-value "--replace-input" args))
	options)
    (unless (or outfile replace-input)
      (error "Must specify either outfile or --replace-input."))
    (setq options
	  (seq-difference args (list (concat "--outfile=" outfile)
				     (concat "--infile=" infile))))
    (let ((call (concat "qpdf" " " infile " "
			(mapconcat
			 (lambda (x)
			   (replace-regexp-in-string
			    "^--custom=" ""
			    (replace-regexp-in-string
			     "^--pages=" "--pages " x)))
			 options
			 " ")
			" " outfile)))
      (message "call: %s" call)
      (call-process-shell-command call))
    (mapcar (lambda (f) (funcall f args))
	    qpdf-run-after-functions)))


(defun qpdf--default-run-after-function (args)
  "Default function to call after the `qpdf' shell command has been run.
Argument `args' contains the transient-args passed down from `qpdf'."
  (let ((replace-input (transient-arg-value "--replace-input" args))
	(outfile (transient-arg-value "--outfile=" args)))
    (when (equal major-mode 'pdf-view-mode)
	(if replace-input
	    (revert-buffer t t)
	  (if (not (file-exists-p outfile))
	      (error "Cannot find outfile.")
	    (let ((dark pdf-view-midnight-minor-mode))
	      (find-file outfile)
	      (when dark
		(pdf-view-midnight-minor-mode))))))))


(defun qpdf-docs (&optional args)
  "Open the qpdf online documentation using `browse-url'."
  (interactive (list (transient-args transient-current-command)))
  (browse-url qpdf-docs-url))


(defun qpdf--read-pages (prompt initial-input history)
  "Calls `qpdf-read-pages-function'."
  (funcall qpdf-read-pages-function prompt initial-input history))


(defun qpdf--default-read-pages-function (prompt initial-input history)
  "Read a page range conditionally providing presets."
  (if (equal major-mode 'pdf-view-mode)
      (qpdf--read-pages-with-presets prompt initial-input history)
    (qpdf--read-pages-without-presets prompt initial-input history)))


(defun qpdf--read-pages-without-presets (prompt initial-input history)
  "Read a page range without providing presets based on current page."
  ;; allow omitting ". " and " --"
  (let ((instring (read-string
		   (concat qpdf-pages-prepromt prompt)
		   initial-input history)))
    (unless (string-match-p
	     "^\\(\\.\\|.*[\\.pdf]\\)" instring)
      (setq instring (concat ". " instring)))
    (unless (string-match-p " --$" instring)
      (setq instring (concat instring " --")))
    instring))


(defun qpdf--read-pages-with-presets (prompt initial-input history)
  "Read a page range while providing some presets based on current page."
  (let* ((current-page (image-mode-window-get 'page))
	 (options `((?f "from current" from-current)
		    (?u "until current" until-current)
		    (?e "except current" except-current)
		    (?c "custom" custom)))
	 (choice-char             
	  (read-char-choice 
	   (mapconcat
	    (lambda (item) (format "%c: %s" (car item) (cadr item))) 
	    options "; ")
	   (mapcar #'car options)))
	 (choice (nth 2 (assoc choice-char options)))
	 (pages (cond ((equal choice 'from-current)
		       (concat ". " (number-to-string current-page)
			       "-z" " --"))
		      ((equal choice 'until-current)
		       (concat ". 1-" (number-to-string current-page) " --"))
		      ((equal choice 'except-current)
		       (cond ((equal current-page (pdf-info-number-of-pages))
			      ". 1-r2 --")
			     ((equal current-page 1) ". 2-z --")
			     (t (concat ". 1-"
					(number-to-string (- current-page 1))
					"," 
					(number-to-string (+ current-page 1))
					"-z --"))))
		      ((equal choice 'custom)
		       ;; allow omitting ". " and " --"
		       (let ((instring (read-string
					(concat qpdf-pages-prepromt prompt)
						    initial-input history)))
			 (unless (string-match-p
				  "^\\(\\.\\|.*[\\.pdf]\\)" instring)
			   (setq instring (concat ". " instring)))
			 (unless (string-match-p " --$" instring)
			   (setq instring (concat instring " --")))
			 instring)))))
    (message "")
    pages))


(defun qpdf--read-file (prompt _initial-input _history)
  "Read a file."
  (file-truename (read-file-name prompt)))


(defun qpdf--read-custom (prompt initial-input history)
  "Read custom options."
  (read-string (concat qpdf-custom-prepromt prompt)
	       initial-input history))


(defun qpdf--make-unique-filename (filename)
  "Increment `filename' until it is unique."
  (let ((extension (file-name-extension filename))
	(num 1)
	new-filename)
    (if (file-exists-p filename)
	(progn 
	  (setq new-filename
		(concat (file-name-sans-extension filename)
			"-" (number-to-string num)))
	  (while (file-exists-p (concat new-filename "." extension))
	    (setq num (+ num 1))
	    (setq new-filename
		  (concat (file-name-sans-extension filename)
			  "-" (number-to-string num))))
	  (setq filename (concat new-filename "." extension)))
      filename)))


(provide 'qpdf.el)
;;; qpdf.el ends here
