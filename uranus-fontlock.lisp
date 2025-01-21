;;; -*- mode: Lisp; coding: utf-8  -*-

(in-package "EDITOR")

(unless-defined defmacro unless-defined (def name args &body body) 
  (unless (or (fboundp name) (special-operator-p name) (macro-function name))
    `(,def ,name ,args ,@body)))

(defparameter *uranus-mode-special-operators-fsa*
  (make-fsa
   (string-append
    "("
    (regexp-opt '("define"
                  "assert"
                  "asserta"
                  "assertz"
                  "assertq"
                  "="
                  "match"
                  "matchq"
                  "=q="
                  "eq"
                  "=="
                  "eqq"
                  "=eq="
                  "deny"
                  "denyq"
                  "retract"
                  "set"
                  "cut"
                  "fail"
                  "not"
                  "do"
                  )
                t)
    "\\>")))


(defparameter *uranus-mode-control-structures-fsa*
  (make-fsa
   (string-append
    "("
    (regexp-opt '("or"
                  "and"
                  "dand"
                  "dor"
                  "return"
                  "select"
                  "with")
                t)
    "\\>")))


(defparameter *uranus-mode-constant-fsa*
  (make-fsa
   (string-append
    "("
    (regexp-opt '("true" "false")
                t)
    "\\>")))


(unless-defined defun move-syntax-table (move-syntax)
  (slot-value move-syntax 'editor::table))

(defun uranus-font-lock-fontify-keywords-region (start end)
  (let ((buffer (point-buffer start)))
    (with-locked-buffer-no-modification buffer
      (let* ((table (move-syntax-table (buffer-syntax-table buffer))))
        (with-point ((point start :temporary)
                     (form-start start :temporary)
                     (form-end start :temporary))
          (let ((pattern (load-time-value (make-fsa "^(def[^ 	]*[ 	]+"))))
            (loop (let ((found-len (find-regexp-pattern point pattern t end)))
                    (unless found-len
                      (return))
                    (point-after point)
                    (move-point form-end point)
                    (unless
                        (and
                         (form-offset form-end 1)
                         (progn
                           (when (looking-at "defmethod\\|defgeneric"
                                             point)
                             (font-lock-apply-highlight point form-end *font-lock-keyword-face*))
                           (let ((name-face
                                  (cond ((looking-at "defmethod\\|defgeneric"
                                                     point)
                                         *font-lock-function-name-face*)
                                        ((looking-at "defparameter"
                                                     point)
                                         *font-lock-variable-name-face*)
                                        (t
                                         *font-lock-type-face*))))
                             (character-offset point (1- found-len))
                             (move-point form-start point)
                             (let ((setf-len (looking-at "(setf[ 	]+" point)))
                               (cond (setf-len
                                      (point-after form-start)
                                      (character-offset point setf-len))
                                     ((at-open-p point table)
                                      (point-after form-start)
                                      (point-after point)))
                               (loop (unless (at-symbol-character-p point table)
                                       (return))
                                     (point-after point))
                               (when setf-len
                                 (unless (at-close-p point table)
                                   (move-point form-start point)))
                               (when (point> point form-start)
                                 (font-lock-apply-highlight form-start point name-face))))
                           (point-before point)
                           (form-offset point 1)))
                      (unless (line-offset point 1 0)
                        (return))))))))
      (with-point ((point start :temporary)
                   (form-start start :temporary))
        (let ((pre-pattern-buf-start (load-time-value (make-fsa ".[^\")('`,]")))
              (pre-pattern (load-time-value (make-fsa "[^][&$%^!?><.{}~+=_@*a-z0-9:#-].[^\")('`,]")))
              (pattern (load-time-value (make-fsa "[:&]"))))
          (loop (cond ((not (find-regexp-pattern point pattern t end))
                       (return))
                      ((if (point-before point)
                           (and (looking-at pre-pattern point)
                                (progn
                                  (point-after point)
                                  t))
                           (looking-at pre-pattern-buf-start point))
                       (move-point form-start point)
                       (let ((face (if (eql (next-character form-start) #\:)
                                       *font-lock-builtin-face*
                                       *font-lock-type-face*)))
                         (flet ((boring-face-p (face1 face2)
                                  (declare (ignore face1))
                                  (or (eq face2 nil)
                                      (eq face2 face))))
                           (declare (dynamic-extent #'boring-face-p))
                           ;; MJS 30Mar01: checking the face here avoids problems with
                           ;; form-offset on malformed things inside comments etc (already
                           ;; given a face).
                           (if (boring-face-p :dont-care (get-text-property form-start 'face))
                               (progn

                                 (unless  (form-offset point 1)
                                   (return))
                                 (font-lock-apply-highlight form-start point face))
                               (unless (text-property-any point 'face :dont-care
                                                          :test #'boring-face-p)
                                 (return))))))
                      (t (unless (character-offset point 2) (return)))))))
      (with-point ((point start :temporary)
                   (form-start start :temporary))
        (let ((pattern *uranus-mode-special-operators-fsa*))
          (loop (let ((length (find-regexp-pattern point pattern t end)))
                  (unless length
                    (return))
                  (move-point form-start point)
                  (point-after form-start)
                  (character-offset point length)
                  (font-lock-apply-highlight form-start point *font-lock-special-operator-face*)))))
      (with-point ((point start :temporary)
                   (form-start start :temporary))
        (let ((pattern *uranus-mode-control-structures-fsa*))
          (loop (let ((length (find-regexp-pattern point pattern t end)))
                  (unless length
                    (return))
                  (move-point form-start point)
                  (point-after form-start)
                  (character-offset point length)
                  (font-lock-apply-highlight form-start point *font-lock-keyword-face*)))))
      (with-point ((point start :temporary)
                   (form-start start :temporary))
        (let ((pattern *uranus-mode-constant-fsa*))
          (loop (let ((length (find-regexp-pattern point pattern t end)))
                  (unless length
                    (return))
                  (move-point form-start point)
                  (point-after form-start)
                  (character-offset point length)
                  (font-lock-apply-highlight form-start point *font-lock-keyword-face*))))))))


(unless-defined defun lisp-font-lock-mark-block-function (point other-point)
  (get-defun-start-and-end-points point point other-point))


(setf (variable-value 'font-lock-fontify-syntactically-region-function
                      :mode "Uranus")
      #'lisp-font-lock-fontify-syntactically-region)


(setf (variable-value 'font-lock-fontify-keywords-region-function
                      :mode "Uranus")
      #'uranus-font-lock-fontify-keywords-region)


(setf (variable-value 'font-lock-mark-block-function
                      :mode "Uranus")
      #'lisp-font-lock-mark-block-function)


(setf (variable-value 'font-lock-fontify-by-default
                      :mode "Uranus")
      T)


(add-global-hook uranus-mode-hook 'turn-on-font-lock)


;;; *EOF*
