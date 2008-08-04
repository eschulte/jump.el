;;; jump.el --- jump between buffer

;;; Idea: to subsume ffip and toggle had a 

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

;; - the idea of having a "root"
;; - the ability to subsume ffip
;; - the bulk of rinari-jump "maybe with the addition of documentation
;;   string to rule sets"

;;; Code:
(require 'which-func)
(require 'inflections) ;; try plural/singular versions by default

(defun jump-root (&optional dir)
  "Find the root of the project defined by presence of `.emacs-project'."
  (file-name-directory (plv-find-project-file default-directory "")))

(defcustom jump-method-command
  'which-function
  "Function used to determine the current function.  Defaults to
`which-function', but others may be preferable, for example
`ruby-add-log-current-method' is more reliable in ruby code.")

(defun jump-method ()
  (let ((func (funcall jump-method-command)))
    (or (and (string-match "#\\(.+\\)" func) (match-string 1 func))
	func)))

(defun jump-create (path)
  "Function used to create the target of a jump if it is
missing."
  (find-file path))

;; jump from a spot
(defun jump-from (spec)
  )

;; jump to a spot
(defun jump-to (spec)
  )

;; jump to a specified path (regexp OK)
(defun jump-to-path (path)
  "Jump to the location specified by PATH (regexp allowed in
path).  If path ends in / then just look in that directory"
  (let ((default-directory (jump-root))
	method method-point)
    (when (string-match "^\\(.*\\)#\\(.*\\)$" path)
      (setf method (match-string 2 path))
      (setf path (match-string 1 path)))
    (if (if (string-match "/$" path)
	    (ido-find-file-in-dir path)
	  (if (file-exists-p path)
	      (find-file path)
	    (let ((base (reverse (split-string path "/"))) ;; search with regexp
		  dir matcher files found-file)
	      (while (and (null found-file) (consp base))
		(setf matcher (car base)) (setf base (cdr base)) ;; drop down a dir
		(setf dir (concat (mapconcat 'identity (reverse base) "/") "/"))
		(setf files (delq nil (mapcar
				       (lambda (file) ;; check for matches to regexp
					 (if (string-match (concat "^" matcher) file)
					     file))
				       (directory-files dir))))
		(case (length files)
		  (0 )
		  (1 (setf found-file (car files)))
		  (t (setf found-file (ido-completing-read dir files)))))
	      (if found-file (find-file (concat dir found-file))))))
	(when method ;; if file was found, then find method
	  (goto-char (point-min))
	  (while (and (forward-line) (null method-point))
	    (if (string-match method (or (jump-method) ""))
		(setf method-point (point))))
	  (if method-point
	      (goto-char method-point)
	    (goto-char (point-min)))))))

;; handle the jump
(defun jumper ())

(provide 'jump)
;;; jump.el ends here