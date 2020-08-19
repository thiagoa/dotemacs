;;; environment.el --- Environment variables
;;
;;; Commentary:

;;; Code:

(setenv "LESS_OPEN" nil)
(setenv "VISUAL" "emacsclient")
(setenv "EDITOR" (getenv "VISUAL"))
(setenv "PAGER" "cat")
(setenv "TERM" "xterm-256-color")
(setenv "SKIP_COVERAGE" "1")

;;; environment.el ends here
