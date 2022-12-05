;;; qpdf.el --- A transient Emacs wrapper for qpdf -*- lexical-binding: t; -*-

;;; Commentary:

;; This requires that qpdf is installed and in your path.
;; Linux/Windows: https://github.com/qpdf/qpdf/releases/
;; MacOS: https://formulae.brew.sh/formula/qpdf

;;; Code:

(require 'transient)


(defgroup qpdf.el nil
  "A transient Emacs wrapper for qpdf."
  :group 'multimedia)

(defcustom qpdf-transient-non-suffix 'transient--do-stay
  "Function which controls behavior when pressing keys not part of transient.
With `transient--do-stay' run the bound commands while persisting the
transient. Set to nil to do nothing but show a message."
  :group 'qpdf.el
  :type 'function)

(defcustom qpdf-docs-url "https://qpdf.readthedocs.io/en/stable/cli.html"
  "The url used by `qpdf-docs'."
  :group 'qpdf.el
  :type 'string)

(defcustom qpdf-pages-prepromt
  "Syntax: 'file [--password=password] [page-range] [...] --'.
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
  :type 'function)

(defcustom qpdf-read-pages-function 'qpdf--default-read-pages-function
  "Function used to read the `--pages=' argument.
Should take three arguments: prompt initial-input history and output a string."
  :group 'qpdf.el
  :type 'function)

(defcustom qpdf-run-after-functions '(qpdf--default-run-after-function)
  "List of functions to call after the qpdf shell command has been run.
Each should take one argument, the transient args passed down from `qpdf'."
  :group 'qpdf.el
  :type 'hook)

(defcustom qpdf-display-call t
  "If non-nil, display a message with the qpdf call."
  :group 'qpdf.el
  :type 'boolean)

(defcustom qpdf-default-outfile "qpdf-outfile.pdf"
  "Name for the default outfile."
  :group 'qpdf.el
  :type 'string)

(defcustom qpdf-open-output-file t
  "If non-nil open the output file produced by the qpdf shell command in Emacs.
The output file is either the --outfile specified in the `qpdf' transient or if
--replace-input is specified it is the specified --infile."
  :group 'qpdf.el
  :type 'boolean)

;;;###autoload
(defcustom qpdf-prefix-groups
  (list
   ["General"
    ("p" "pages" "--pages=" qpdf--read-pages)
    ("i" "infile" "--infile=" qpdf--read-file)
    ("o" "outfile" "--outfile=" qpdf--read-file)
    ("r" "replace input" "--replace-input")
    (qpdf--flatten-annotations)
    ("d" "password" "--password=" :level 5)
    ("c" "custom" "--custom=" qpdf--read-custom)]
   ["Modification"
    :hide (lambda ()
            (not (eq (car transient--redisplay-key)
                     ?m)))
    ("m c" "" "--collate="
     (lambda (prompt initial-input history)
       (read-string (concat "Collate pages in groups of: ")
		    initial-input history)))
    ("m r" "" "--rotate="
     (lambda (prompt initial-input history)
       (read-string (concat "Syntax: [+|-]angle[:page-range]\n" prompt)
		    initial-input history)))
    ("m s" "" "--split-pages="
     (lambda (prompt initial-input history)
       (read-string (concat "Split pages into groups of: ")
		    initial-input history)))]
   [["Actions"
     ("<return>" " qpdf-run" qpdf-run)]
    [""
     ("h" "qpdf-docs" qpdf-docs :transient t)]])
  "List of vectors as expected for the GROUPs in `transient-define-prefix`.

GROUPs add key bindings for infix and suffix commands and specify how these
bindings are presented in the popup buffer of the `qpdf' command. At least
one GROUP has to be specified. See info node `(transient)Binding Suffix and
Infix Commands'."
  :group 'qpdf.el
  :type '(repeat sexp))

;;;###autoload
(defcustom qpdf-incompatible '(("--replace-input" "--outfile="))
  "List of incompatible options in the `qpdf' transient."
  :group 'qpdf.el
  :type '(repeat (repeat string)))


;;;###autoload (autoload 'qpdf "qpdf.el" "" t)
(transient-define-prefix qpdf ()
  "Transient dispatcher for the qpdf shell command.
See URL `https://qpdf.readthedocs.io/en/stable/cli.html#page-selection'
for details on the --pages argument and others."
  :init-value 'qpdf--set-defaults
  :transient-non-suffix 'qpdf-transient-non-suffix
  :incompatible qpdf-incompatible
  qpdf-prefix-groups)


;;;###autoload
(defun qpdf--set-defaults (obj)
  "Set dynamic initial values for object OBJ."
  (oset obj value (funcall qpdf-set-defaults-function)))


(defun qpdf--default-set-defaults-function ()
  "Default function used to set `qpdf' defaults."
  `(,(if (or (equal major-mode 'doc-view-mode)
	     (equal major-mode 'pdf-view-mode))
	 (concat "--pages="
		 (concat ". " (number-to-string
			       (image-mode-window-get 'page))
			 " --")
		 nil))
    ,(concat "--infile="
	     (cond ((equal major-mode 'doc-view-mode)
		    (buffer-file-name))
		   ((equal major-mode 'pdf-view-mode)
		    (pdf-view-buffer-file-name))
		   (t "--empty")))
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
  (let ((pages (transient-arg-value "--pages=" args))
	(infile (transient-arg-value "--infile=" args))
	(outfile (transient-arg-value "--outfile=" args))
	(replace-input (transient-arg-value "--replace-input" args))
	options)
    (unless (or outfile replace-input)
      (error "Must specify either outfile or --replace-input"))
    (setq options
	  (seq-difference args (list (concat "--outfile=" outfile)
				     (concat "--infile=" infile))))
    (let ((call (concat "qpdf" " '" infile "' "
			(mapconcat
			 (lambda (x)
			   (replace-regexp-in-string
			    "^--custom=" ""
			    (replace-regexp-in-string
			     "^--pages=" "--pages " x)))
			 options
			 " ")
			(when outfile
              (concat " '" outfile "'")))))
      (when qpdf-display-call
	(message "call: %s" call))
      (call-process-shell-command call))
    (mapcar (lambda (f) (funcall f args))
	    qpdf-run-after-functions)))


(defun qpdf--default-run-after-function (args)
  "Default function to call after the `qpdf' shell command has been run.
Argument ARGS contains the transient args passed down from `qpdf'."
  (let ((replace-input (transient-arg-value "--replace-input" args))
	(outfile (transient-arg-value "--outfile=" args))
	(infile (transient-arg-value "--infile=" args)))
    (when (or (and outfile (not (file-exists-p outfile)))
	      (and replace-input (not (file-exists-p infile))))
      (error "Cannot find qpdf output file"))
    (if (and (or (equal major-mode 'doc-view-mode)
		   (equal major-mode 'pdf-view-mode))
	       replace-input)
	(revert-buffer t t)
      (when qpdf-open-output-file
	(when replace-input
	  (setq outfile infile))
	(let ((dark (bound-and-true-p pdf-view-midnight-minor-mode)))
	  (find-file outfile)
	  (when dark
	    (pdf-view-midnight-minor-mode)))))))


(defun qpdf-docs (&optional args)
  "Open the qpdf online documentation using `browse-url'."
  (interactive (list (transient-args transient-current-command)))
  (browse-url qpdf-docs-url))


(defun qpdf--read-pages (prompt initial-input history)
  "Calls `qpdf-read-pages-function'."
  (funcall qpdf-read-pages-function prompt initial-input history))


(defun qpdf--default-read-pages-function (prompt initial-input history)
  "Read a page range conditionally providing presets."
  (if (or (equal major-mode 'doc-view-mode)
	  (equal major-mode 'pdf-view-mode))
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


(defun qpdf--read-pages-with-presets (prompt initial-input history
					     &optional choice)
  "Read a page range while providing some presets based on current page.
Optionally, CHOICE can already pre-specify the preset option to choose."
  (let ((current-page (image-mode-window-get 'page))
	(final-page (cond ((equal major-mode 'doc-view-mode)
			   (doc-view-last-page-number))
			  ((equal major-mode 'pdf-view-mode)
			   (pdf-info-number-of-pages))
			  (t (error
			      (concat "`qpdf--read-pages-with-presets' can "
				      "only be run when in doc-view-mode or"
				      " pdf-view-mode.")))))
	(options `((?f "from current" from-current)
		   (?u "until current" until-current)
		   (?e "except current" except-current)
		   (?c "custom" custom)))
	choice-char pages)
    (unless choice
      (setq choice-char
	    (read-char-choice
	     (mapconcat
	      (lambda (item) (format "%c: %s" (car item) (cadr item)))
	      options "; ")
	     (mapcar #'car options)))
      (setq choice (nth 2 (assoc choice-char options))))
    (setq pages (cond ((equal choice 'from-current)
		       (concat ". " (number-to-string current-page)
			       "-z" " --"))
		      ((equal choice 'until-current)
		       (concat ". 1-" (number-to-string current-page) " --"))
		      ((equal choice 'except-current)
		       (cond ((equal current-page final-page)
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
			 instring))))
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
  "Increment FILENAME until it is unique."
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
