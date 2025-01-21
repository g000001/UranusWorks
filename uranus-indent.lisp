;; -*- Mode: Lisp; -*-

(in-package "EDITOR")

(defun setup-uranus-indent (form-name no-of-args &optional standard special flag-slot)
  (defindent *uranus-syntax-table* form-name no-of-args standard special flag-slot))

(setup-uranus-indent "define" 1 2)
(setup-uranus-indent "select" 1 2)
(setup-uranus-indent "and" nil)
(setup-uranus-indent "dand" nil)
(setup-uranus-indent "or" nil)
(setup-uranus-indent "dor" nil)
(setup-uranus-indent "do" nil)
(setup-uranus-indent "with" 1 2)
(setup-uranus-indent "within" 1 2)


;;; *EOF*
