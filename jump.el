;;; jump.el --- jump between buffers

;;; Idea: to subsume ffip and toggle, which were both big influences

;;; TODO:

;;: TODO:

;; break this down into component parts, write unit tests, and test these parts

;; - jump-from (takes either a string, a function, or t and
;;   returns the pieces to be passed to the jump-to function)
;; 
;; - jump-to (takes a string, or a function and the pieces passed from
;;   the jump-to function and opens the related buffer)
;;
;; - jumper (the function which calls jump-from and jump-to, and
;;   passed the required arguments between them)
;;
;; - for every set of rules (controller, model, etc...) return a
;;   function defined by that rule set.  This seems to be the most
;;   easy to bind/use

;; things to keep

;; - the idea of having a "root" (string or function)
;; - the ability to subsume ffip
;; - the bulk of rinari-jump "maybe with the addition of documentation
;;   string to rule sets"

;;; Code:
(require 'which-func)
(require 'inflections) ;; try plural/singular versions by default

(defvar jump-ignore-file-regexp
  "\\\"\\(.*\\(git\\|svn\\|cvs\\).*\\|.*~\\|.*\\#.*\\#\\)\\\""
  "regexp for the find shell command to ignore undesirable files")

(defvar jump-root
  nil
  "Set to a path to override the `jump-root' function.")

(defun jump-root (&optional dir)
  "Find the root of the project defined by presence of `.emacs-project'."
  (file-name-directory (plv-find-project-file default-directory "")))

(defcustom jump-method-command
  'ruby-add-log-current-method
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

(defun flatten (input)
  (if input
      (if (consp input)
	  (append (flatten (car input))
		  (flatten (cdr input)))
	(list input))))

(defun jump-recursive-directory-files (directory)
  (split-string (shell-command-to-string
		 (concat "find " directory " -type f -not -regex "
			 jump-ignore-file-regexp))))

(defun jump-select-and-find-file (files)
  "Select a single file from a list.  Return the path selected or
nil if files was empty."
  (let ((file   (case (length files)
		  (0 nil)
		  (1 (car files))
		  (t (ido-completing-read "Jump to: " files)))))
    (if file (find-file file))))

(defun jump-to-file (&optional file)
  "Open the file located at file if file ends in a / then look in
the related directory, and if file contains regexps then select
from all matches."
  (interactive "Mfile: ")
  (let ((default-directory (or jump-root
			       (jump-root))))
    (if (string-match "/$" file) ;; open directory
	(ido-find-file-in-dir file)
      (if (file-exists-p file) ;; open file
	  (find-file file)
	(jump-select-and-find-file ;; open with regexp
	 (let ((base (reverse (split-string file "/"))) dir matcher)
	   (delete-dups
	    (loop for subdir in base ;; for incrementally less directories
		  do (setf matcher (car base)) (setf base (cdr base))
		  (setf dir (concat (mapconcat 'identity (reverse base) "/") "/"))
		  collect (delq nil
				(mapcar ;; for all files under directory
				 (lambda (el)
				   (if (string-match matcher el) ;; match regexp
				       (concat dir "/" el)))
				 (jump-recursive-directory-files dir)))))))))))

(defun jump-to-method (&optional method)
  "If `jump-method' returns method in buffer, go to the first
line inside of method."
  (interactive "Mmethod: ")
  (goto-char (point-min))
  (while (not (or (string-equal (jump-method) method)
		  (and (> (forward-line 1) 0)
		       (goto-char (point-min)))))))

;; facade functions
(defun jump-from (spec)
  "jump from a spot"
  )

(defun jump-to (spec)
  "jump to a spot"
  )

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
      (message (format "no file found for %s" path)))))

;; ;;; old version
;; (defun jump-to-path (path)
;;   "Jump to the location specified by PATH (regexp allowed in
;; path).  If path ends in / then just look in that directory"
;;   (let ((default-directory (jump-root))
;; 	method method-point)
;;     (when (string-match "^\\(.*\\)#\\(.*\\)$" path)
;;       (setf method (match-string 2 path))
;;       (setf path (match-string 1 path)))
;;     (if (if (string-match "/$" path) ;; open directory
;; 	    (ido-find-file-in-dir path)
;; 	  (if (file-exists-p path) ;; open file
;; 	      (find-file path)
;; 	    (let ((base (reverse (split-string path "/"))) ;; search with regexp
;; 		  dir matcher files found-file)
;; 	      (while (and (null found-file) (consp base))
;; 		(setf matcher (car base)) (setf base (cdr base)) ;; drop down a dir
;; 		(setf dir (concat (mapconcat 'identity (reverse base) "/") "/"))
;; 		(setf files (delq nil (mapcar
;; 				       (lambda (file) ;; check for matches to regexp
;; 					 (if (string-match (concat "^" matcher) file)
;; 					     file))
;; 				       (directory-files dir))))
;; 		(case (length files)
;; 		  (0 )
;; 		  (1 (setf found-file (car files)))
;; 		  (t (setf found-file (ido-completing-read dir files)))))
;; 	      (if found-file (find-file (concat dir found-file))))))
;; 	(when method ;; if file was found, then find method
;; 	  (goto-char (point-min))
;; 	  (while (and (forward-line) (null method-point))
;; 	    (if (string-match method (or (jump-method) ""))
;; 		(setf method-point (point))))
;; 	  (if method-point
;; 	      (goto-char method-point)
;; 	    (goto-char (point-min)))))))

;; handle the jump
(defun jumper ())

(provide 'jump)
;;; jump.el ends here