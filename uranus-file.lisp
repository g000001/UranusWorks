;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "EDITOR")

(define-file-type-hook ("ura") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Uranus")
  (when (find-package "https://github.com/g000001/lw-paredit")
    (setf (buffer-minor-mode buffer "Paredit") T)
    (turn-on-font-lock buffer T))
  (let ((pathname (buffer-pathname buffer)))
    (unless (and pathname (probe-file pathname))
      (insert-string
       (buffer-point buffer)
       (format nil
               ";;; -*- mode: Uranus; coding: utf-8  -*-~2%~A~2%"
               "\"(uranus:uranus)\"")))))
