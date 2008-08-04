;;; jump.el --- easily specify functions for jumping between files

;; Copyright (C) 2008 Eric Schulte

;; Author: Eric Schulte
;; URL: 
;; Version: 0.0
;; Created: 2008-08-01
;; Keywords: project, convenience, file, navigation, extension, toggle
;; EmacsWiki: 

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Attribution:

;; Inspired by toggle.el: borrows ideas and fragments of code from
;; toggle.el Copyright (C) 2006-2007 by Ryan Davis

;;; Commentary:

;; Provides a means for easily specifying movement between files.
;; This is similar to toggle.el, but it has many more knobs to be
;; turned.

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

(defcustom jump-which-function-command
  'which-function
  "Function used to determine the current function.  Defaults to
`which-function', but others may be preferable, for example
`ruby-add-log-current-method' is more reliable in ruby code.")

(defun jump-which-function ()
  (let ((func (funcall jump-which-function-command)))
    (or (and (string-match "#\\(.+\\)" func) (match-string 1 func))
	func)))

(defvar jump-default-directory
  ""
  "The directory which serves as the pivot point for jumps.  It
is updated as the beginning of the match in the jump pitching
function.")

(defvar jump-mapping-styles '()
  "List of Rules used to jump between files.  Each rule should be of the form")

(defun jump-project-root (&optional dir)
  "Find the root of the project defined by presence of `.emacs-project'."
  (require 'project-local-variables)
  (file-name-directory (plv-find-project-file default-directory "")))

(defun jump-to-path (path)
  "Jump to the location specified by PATH (regexp allowed in
path).  If path ends in / then just look in that directory"
  (let (method method-point)
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
	    (if (string-match method (or (jump-which-function) ""))
		(setf method-point (point))))
	  (if method-point
	      (goto-char method-point)
	    (goto-char (point-min)))))))

(defun jump-string-to-pitcher (string)
  "Take a string and return a function which matches the string
against the current placement (point and buffer) updates
`jump-default-directory' and returns a list of the matched places
or nil."
  `(lambda ()
     (let ((rule ,(replace-regexp-in-string
		   "\\\\[[:digit:]]" "\\\\(.*\\\\)"
		   (replace-regexp-in-string ; special case for "\\1.ext"
		    "^\\\\[[:digit:]]" "\\\\([^/]*\\\\)" string)))
	   (place-string ,(if (string-match "#" string)
			      '(concat (buffer-file-name) "#" (jump-which-function))
			    '(buffer-file-name)))
	   (counter 1)
	   output mymatch)
       (message "inside")
       (message (format "%s" place-string))
       (message (format "%S" rule))
       (message "outside")
       (string-match rule place-string)
       (while (match-string counter place-string)
	 (setf mymatch (match-string counter place-string))
	 (setf output (cons mymatch output))
	 (setf counter (+ 1 counter)))
       (message (format "%S" output))
       output)))

(defun jump-string-to-catcher (string)
  "Takes STRING, and returns a function which accepts any number
of arguments and for each argument replaces \"\\\\nth\" in string
with the nth argument."
  `(lambda (&rest replacments)
     (let ((string ,string)
	   (count 1)
	   rep)
       (if (listp replacements)
	   (while (setf rep (nth (- count 1) replacements))
	     (setf string (replace-regexp-in-string (format "\\\\%d" count) rep string))
	     (setf count (+ 1 count))))
       string)))

(defun jump-default-creation-function (path &rest others)
  "Default function used to create the target of a jump if it is
missing."
  (find-file path))

(defun jump-expand (rules)
  "Expand the rules info functions"
  (mapcar 
   (lambda (mapping)
     (list (car mapping)
	   (mapcar 
	    (lambda (rule)
	      (cons
	       (if (stringp (car rule))
		   (jump-string-to-pitcher (car rule))
		 (car rule))
	       (if (stringp (cdr rule))
		   (jump-string-to-catcher (cdr rule))
		 (cdr rule))))
	    (cadr mapping))
	   (or (caadr mapping)
	       'jump-default-creation-function)))
   rules))

(defvar jump-mappings (jump-expand jump-mapping-styles)
  "Expansion of rules in `jump-mapping-styles'")

(defun jump (&optional mapping create)
  "Try to jump from current position following the rules defined
by MAPPING in `jump-mappings'.  With an optional prefix argument
create the destination if it is missing."
  (interactive)
  (let* ((default-directory (or (and (functionp jump-project-root) (funcall jump-project-root))
				jump-project-root))
	 (mapping (or mapping
		      (completing-read "Mapping: "
				       (mapcar (lambda (key)
						 (format "%S" (car key)))
					       jump-mappings))))
	 (rules (assoc (intern mapping) jump-mappings))
	 replacements path)
    (message "looping")
    (loop for rule in (cadr rules) ;; try every rule in mappings
	  until (and (setf replacements (eval (list (car rule))))
		     (setf path (eval (append (list (cdr rule)) replacements))))
	  do (message (format "-%S-" (car rule)))
	  )
    (message "looped")
    (if path (or (jump-to-path path)
		 (if create ;; use the function specified in mapping to create
		     (eval (list (caddr mapping) path))
		   (setf path nil))))))

(provide 'jump)
;;; jump.el ends here