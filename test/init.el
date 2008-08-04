(cd (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path "../")
(add-to-list 'load-path "./")
(add-to-list 'load-path "~/emacs/loads/ruby-mode")
(require 'jump)
(require 'elunit)
(require 'ido)
(require 'ruby-mode)
(ido-mode t)

;; test jumping
(defsuite jump-suite nil
  :setup-hook (lambda () )
  :teardown-hook (lambda ()
		   (switch-to-buffer "*Messages*")
		   (message "test completed")))

(deftest jump-to-path-test jump-suite
  ;; test moving from everywhere to everywhere
  (save-excursion
    (unless (y-or-n-p "allways select the first option ok? ")
      (error "if you won't co-operate, I won't run these tests"))
    (cd "./jump-fake-app")
    (flet ((jump-root () "~/projects/jump/test/jump-fake-app/")
	   (jump-method () (ruby-add-log-current-method))
	   (jumpit (path end)
		   ;; go there
		   (jump-to-path path)
		   ;; assert where there is
		   (message (format "asserting at %S" end))
		   (assert-equal (file-name-nondirectory (car end))
				 (file-name-nondirectory (buffer-file-name)))
		   (assert-equal (cdr end) (point))
		   ;; clean up
		   (kill-buffer (file-name-nondirectory (car end)))))
      (jumpit "animals/.*" '("chicken.rb" . 1))
      (jumpit "animals/pig.rb#stomach" '("pig.rb" . 28))
      ;; - failing this test because it needs to start looking from the jump-root
      ;; - also, needs to gracefully fail (return nil) when nothing matches the regexp
      (jumpit ".*pork.rb#cook_butt" '("pork.rb" . 69))
      )))

;; simple test
;; (flet ((jump-root () "~/projects/jump/test/jump-fake-app/"))
;;   (jump-to-path "foods/.*.rb"))

(elunit "jump-suite")
