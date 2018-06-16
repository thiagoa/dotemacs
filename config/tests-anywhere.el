(cl-defstruct tests-anywhere-command directory function)

(defvar tests-anywhere-state
  (make-hash-table)
  "A hash with the last command for each test run action")

(defun tests-anywhere-register-and-run (key function)
  (let ((command (make-tests-anywhere-command
                  :directory (projectile-project-root)
                  :function function)))
    (puthash key command tests-anywhere-state))
  (tests-anywhere-run-registered key))

(defun tests-anywhere-run-registered (key)
  (let* ((state (gethash key tests-anywhere-state))
         (default-directory (tests-anywhere-command-directory state)))
    (if state
        (funcall (tests-anywhere-command-function state))
      (message "No prior test run"))))

(defun tests-anywhere-rerun ()
  (interactive)
  (let ((func (tests-anywhere-rerun-get-function)))
    (if func
        (tests-anywhere-register-and-run 'rerun func)
      (tests-anywhere-run-registered 'rerun))))

(defun tests-anywhere-rerun-get-function ()
  (pcase (projectile-project-type)
    ('rails-rspec 'rspec-rerun)
    ('elixir 'alchemist-mix-rerun-last-test)
    ('lein-test (lambda () (cider-test-run-loaded-tests nil)))))

(defun tests-anywhere-verify-all ()
  (interactive)
  (let ((func (tests-anywhere-verify-all-get-function)))
    (if func
        (tests-anywhere-register-and-run 'verify-all func)
      (tests-anywhere-run-registered 'verify-all))))

(defun tests-anywhere-verify-all-get-function ()
  (pcase (projectile-project-type)
    ('rails-rspec 'rspec-verify-all)
    ('elixir 'alchemist-mix-test)
    ('lein-test (lambda () (cider-test-run-project-tests nil)))))
