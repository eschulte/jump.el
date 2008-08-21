;;; jump.el --- build functions for contextually jumping between files

;;; Components

;; - defjump (define functions which call jump-from and jump-to, and
;;   pass the required arguments between them)
;;   
;; - jump-from (takes either a string, a function, or t and
;;   returns the pieces to be passed to the jump-to function)
;; 
;; - jump-to (takes a string, or a function and the pieces passed from
;;   the jump-to function and opens or creates the related buffer)
;;

;;; things to keep

;; - the idea of having a "root" (string or function)
;; - the ability to subsume ffip
;; - the bulk of rinari-jump "maybe with the addition of documentation
;;   string to rule sets"

;;; TODO:

;; - regexp for method

;;; Code:
(require 'which-func)
(require 'findr)
;; (require 'inflections) ;; TODO try plural/singular versions by default

(defvar jump-ignore-file-regexp
  "\\\"\\(.*\\(git\\|svn\\|cvs\\).*\\|.*~\\|.*\\#.*\\#\\)\\\""
  "regexp for the find shell command to ignore undesirable files")

(defvar jump-root nil "Set to a path to override the `jump-root' function.")

(defvar jump-indicator-file ".git"
  "File used to find the base of the current project.")

(defun jump-root (&optional dir)
  "Find the root of the project current project by searching up
directories looking for the `jump-indicator-file'.  It is
probably a good idea to override this function or to set either
of the `jump-root' or `jump-indicator-file' variables."
  (let ((f (expand-file-name jump-indicator-file dir))
	(parent (file-truename (expand-file-name ".." dir))))
    (cond ((string= dir parent) nil)
	  ((file-exists-p f) (concat (or dir default-directory) "/"))
	  (t (jump-root parent)))))

(defvar jump-method-command
  'which-function
  "Function used to determine the current function.  Defaults to
`which-function', but others may be preferable, for example
`ruby-add-log-current-method' is more reliable in ruby code.")

(defun jump-method ()
  "Return the method defined at the current position in current
buffer."
  (let ((func (funcall jump-method-command)))
    (or (and func (string-match "#\\(.+\\)" func) (match-string 1 func))
	func)))

(defun jump-create (path)
  "Function used to create the target of a jump if it is
missing."
  (find-file path))

(defun jump-uniqueify (file-cons)
  "Set the car of the argument to include the directory name plus the file name."
  (setcar file-cons
	  (concat (car file-cons) " "
		  (cadr (reverse (split-string (cdr file-cons) "/"))))))

(defun jump-select-and-find-file (files)
  "Select a single file from an alist of file names and paths.
Return the path selected or nil if files was empty."
  (let ((file   (case (length files)
		  (0 nil)
		  (1 (caar files))
		  (t (ido-completing-read "Jump to: "
					  (mapcar 'car files))))))
    (if file (find-file (cdr (assoc file files))))))

(defun jump-to-file (&optional file)
  "Open the file located at file if file ends in a / then look in
the related directory, and if file contains regexps then select
from all matches."
  (interactive "Mfile: ")
  (let ((root (or jump-root (jump-root)))
	(file-cons (cons (file-name-nondirectory file) file))
	file-alist)
    (if (string-match "/$" file)
	(ido-find-file-in-dir (concat root "/" file)) ;; open directory
      (if (file-exists-p file)
	  (find-file file) ;; open file
	(jump-select-and-find-file ;; open with regexp
	 (mapcar (lambda (file)
		   (let ((file-cons (cons (file-name-nondirectory file)
					  (expand-file-name file))))
		     (when (assoc (car file-cons) file-alist)
		       (jump-uniqueify (assoc (car file-cons) file-alist))
		       (jump-uniqueify file-cons))
		     (add-to-list 'file-alist file-cons)
		     file-cons))
		 (findr (car file-cons)
			(concat root "/" (or (file-name-directory
					      (cdr file-cons)) "")))))))))

(defun jump-to-method (&optional method)
  "If `jump-method' returns method in buffer, go to the first
line inside of method."
  (interactive "Mmethod: ")
  (goto-char (point-min))
  (while (not (or (string-equal (jump-method) method)
		  (and (> (forward-line 1) 0)
		       (goto-char (point-min)))))))

(defun jump-to-path (path)
  "Jump to the location specified by PATH (regexp allowed in
path).  If path ends in / then just look in that directory"
  (let ((file path)
	method)
    (when (string-match "^\\(.*\\)#\\(.*\\)$" path)
      (setf method (match-string 2 path))
      (setf file (match-string 1 path)))
    (if (jump-to-file file)
	(when method (jump-to-method method))
      (progn (message (format "no file found for %s" path)) nil))))

(defun jump-to (spec &optional matches make) ;; TODO maybe should build tests
  "Jump to a spot defined by SPEC.  If optional argument MATCHES
replace all '\\n' portions of SPEC with the nth (1 indexed)
element of MATCHES.  If optiona argument MAKE, then create the
target file if it doesn't exist, if MAKE is a function then use
MAKE to create the target file."
  (let ((root (or jump-root (jump-root)))
	(path (if matches
		  (let ((count 1) (new-spec spec) (spec nil))
		    (while (not (equal spec new-spec))
		      (setf spec new-spec)
		      (setf new-spec
			    (replace-regexp-in-string (format "\\\\%d" count)
						      (nth (- count 1) matches)
						      spec))
		      (setf count (+ 1 count)))
		    new-spec) spec)))
    (unless (jump-to-path path)
      (when make
	(message "making the file")
	(when (functionp make) (funcall (list make path)))
	(find-file (concat root (if (string-match "^\\(.*\\)#" path)
				    (match-string 1 path)
				  path)))))))

(defun jump-from (spec) ;; TODO maybe should build tests
  "Match SPEC to the current location returning a list of any matches"
  (cond
   ((stringp spec)
    (let* ((file (or (and (buffer-file-name)
			(expand-file-name (buffer-file-name)))
		   (buffer-name)))
	 (method (jump-method))
	 (path (if (string-match "#.+" spec)
		   (concat file "#" method)
		 file)))
    (and (string-match spec path)
	 (or (let ((counter 1) mymatch matches)
	       (while (setf mymatch (match-string counter path))
		 (setf matches (cons mymatch matches))
		 (setf counter (+ 1 counter)))
	       (reverse matches))
	     t))))
   ((equal t spec) t)))

(defun defjump (name specs &optional make doc)
  "Define NAME as a function with behavior determined by SPECS.
SPECS should be a list of cons cells of the form

   (jump-from-regexp . jump-to-regexp)

the resulting function NAME will then compare the
jump-from-regexps against the current location using `jump-from'
until one matches, at which point any resulting match
information, along with the related jump-to-regexp will be passed
to `jump-to' which will try to land in the correct buffer.

Optional argument MAKE can be used to specify that missing files
should be created.  If MAKE is a function then it will be called
with the file path as it's only argument.  After possibly calling
MAKE `find-file' will be used to open the path.

Optional argument DOC specifies the documentation of the
resulting function."
  ;; function
  (eval
   `(defun ,name (create) ,(or doc "automatically created by `defjump'")
      (interactive "P")
      (let ((root (or jump-root (jump-root)))
	    matches)
	(loop ;; try every rule in mappings
	 for spec in (quote ,specs)
	 until (setf matches (jump-from (car spec)))
	 finally ;; try to jump-to
	 (cond
	  ((equal t matches) (jump-to (cdr spec) nil (if create ,make)))
	  ((consp matches) (jump-to (cdr spec) matches (if create ,make)))))))))


(provide 'jump)
;;; jump.el ends here