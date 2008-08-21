(cd (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path "../")
(add-to-list 'load-path "./")
(add-to-list 'load-path "~/emacs/elisp/ruby/")
(require 'jump)
(require 'elunit)
(require 'ido)
(require 'ruby-mode)
(ido-mode t)

;;--------------------------------------------------------------------------------
;; test suite
(defsuite jump-suite nil
  :setup-hook (lambda ()
		(setq
		 root "~/projects/jump/test/jump-fake-app/"
		 default-directory "~/projects/jump/test/jump-fake-app/"
		 method-command 'ruby-add-log-current-method
		 jump-method-placements
		 '(("foods/pork.rb" "cook_stomach" 32)
		   ("foods/pork.rb" "cook_butt" 69)
		   ("foods/pork.rb" "cook_outer_back" 117)
		   ("foods/pork.rb" "cook_inner_back" 163))
		 jump-path-regexps
		 '(("pork.rb" "foods/pork.rb")
		   ("pork.rb" "foods/")
		   ("pork.rb" "foods/.*")
		   ("pig.rb" "animals/pig.rb")
		   ("pig.rb" "animals/.*g.rb")
		   ("pig.rb" ".*/pig.rb")
		   ("chicken.rb" "animals/chicken.rb")
		   ("chicken.rb" "animals/........rb")
		   ("chicken.rb" "animals/chi.+.rb"))
		 jump-full-paths
		 '(("foods/pork.rb#cook_butt" "pork.rb" 69)
		   ("foods/.*#cook_outer_back" "pork.rb" 117)
		   ("animals/.*g.rb#inner_back" "pig.rb" 203)
		   ("animals/chi.+.rb#gizzards" "chicken.rb" 33))))

  :teardown-hook (lambda ()
		   (switch-to-buffer "*Messages*")
		   (message "test completed")))

;;--------------------------------------------------------------------------------
;; tests
(deftest jump-to-file-test jump-suite ;; test failing but method seems to work
  (message "testing that jump-to-file lands in the correct file")
  (flet ((check-file-after-jump (file regexp)
			      (message (format "%s =~ %s" file regexp))
			      (jump-to-file regexp)
			      (assert-equal
			       (file-name-nondirectory buffer-file-name)
			       file)
			      (kill-buffer file)))
    (check-file-after-jump "chicken.rb" "animals/........rb")
    (check-file-after-jump "chicken.rb" ".*ck.*")
    (check-file-after-jump "pig.rb" "^...\.rb")
    (check-file-after-jump "pig.rb" ".*g\.rb")
    (check-file-after-jump "pork.rb" "foods/.*\.rb")))

(deftest jump-method-test jump-suite
  (message "testing that jump-method returns the correct method")
  (flet ((check-method-at-place (file method target)
				(find-file file)
				(goto-char target)
				(assert-equal (jump-method) method)
				(message (format "%s" (thing-at-point 'line)))
				(kill-buffer (file-name-nondirectory file))))
    (mapcar (lambda (el)
	      (apply 'check-method-at-place el))
	    jump-method-placements)))

(deftest jump-to-method-test jump-suite
  (message "testing jump-to-method")
  (flet ((jump-and-check (file method target)
			 (find-file file)
			 (jump-to-method method)
			 (assert-equal target (point))
			 (message (format "%s" (thing-at-point 'line)))
			 (kill-buffer (file-name-nondirectory file))))
    (mapcar (lambda (el)
	      (apply 'jump-and-check el))
	    jump-method-placements)))

(deftest jump-to-path-test jump-suite
  (message "testing jump-to-path")
  (flet ((jump-to-path-and-check (path file target)
				 (jump-to-path path)
				 (assert-equal
				  (file-name-nondirectory buffer-file-name)
				  file)
				 (assert-equal target (point))
				 (kill-buffer (file-name-nondirectory file))))
    (mapcar (lambda (el)
	      (apply 'jump-to-path-and-check el))
	    jump-full-paths)))

;; (deftest jump-to-path-test jump-suite
;;   ;; test moving from everywhere to everywhere
;;   (save-excursion
;;     (unless (y-or-n-p "allways select the first option ok? ")
;;       (error "if you won't co-operate, I won't run these tests"))
;;     (cd "./jump-fake-app")
;;     (flet ((jump-root () "~/projects/jump/test/jump-fake-app/")
;; 	   (jump-method () (ruby-add-log-current-method))
;; 	   (jumpit (path end)
;; 		   ;; go there
;; 		   (jump-to-path path)
;; 		   ;; assert where there is
;; 		   (message (format "asserting at %S" end))
;; 		   (assert-equal (file-name-nondirectory (car end))
;; 				 (file-name-nondirectory (buffer-file-name)))
;; 		   (assert-equal (cdr end) (point))
;; 		   ;; clean up
;; 		   (kill-buffer (file-name-nondirectory (car end)))))
;;       (jumpit "animals/.*" '("chicken.rb" . 1))
;;       (jumpit "animals/pig.rb#stomach" '("pig.rb" . 28))
;;       ;; - failing this test because it needs to start looking from the jump-root
;;       ;; - also, needs to gracefully fail (return nil) when nothing matches the regexp
;;       (jumpit ".*pork.rb#cook_butt" '("pork.rb" . 69))
;;       )))

;;--------------------------------------------------------------------------------
;; run tests
(elunit "jump-suite")
