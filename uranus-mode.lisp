(in-package "EDITOR")


;;; The command to invoke URANUS Mode.

(defcommand "Uranus Mode" (p)
  "Put current buffer in URANUS Mode." 
  "Put current buffer in URANUS Mode."  
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Uranus"))


(defparameter *uranus-syntax-table*
  (create-syntax-table  :string-escape #\\
                        :d-escape #\#
                        :double-comment #\#
                        :escape #\\
                        :comment #\;
                        :nested t
                        :end-comment #\Newline
                        :second-comment #\|
                        :first-close-comment #\|
                        :second-close-comment #\#
                        :string #\" 
                        :close '(#\) #\} #\])
                        :open '(#\( #\{ #\[)
                        :quote-char #\'
                        :backquote-char #\`
                        :comma-char #\,
                        :string-within-symbol #\|
                        :whitespace '(#\Tab 
                                      #\Space
                                      #\Formfeed
                                      #\Newline
                                      #\Return)))


(defmode "Uranus"
  :major-p t
  :setup-function 'setup-uranus-mode
  ;;:aliases '("Uranus")
  :vars '((Paren-Pause-Period . nil)
	  (Highlight-Matching-Parens . T)
	  (Comment-Start . ";")
	  (Comment-Begin . ";")
	  (Indent-Function . indent-for-lisp)
          (Indent-Region-Function . lisp-indent-region-for-commands)
	  (Check-Mismatched-Parens . :move)
          (Fill-Paragraph-Function . lisp-fill-paragraph)
          (spelling-line-preprocessor . lisp-mode-spelling-preproecssor)
          (spelling-complex-filter . lisp-mode-spelling-complex-filter))
  :syntax-table *uranus-syntax-table*)


;(bind-key "Lisp Insert )" *close-bracket-char-object* :mode "Uranus")


(defun setup-uranus-mode (buffer)
  (unless (variable-value-if-bound 'current-package :buffer buffer)
    (find-in-package buffer)
    ;(font-lock-fontify-buffer buffer)
    (ignore-errors (eval (read-from-string "(uranus:uranus)")))))


(defun uranus-buffer-p (buffer)
  (string= (buffer-major-mode-name buffer) "Uranus"))


(define-editor-mode-variable add-newline-at-eof-on-writing-file "uranus" T)


(ura:toplevel-execute '(tak 18 12 6 *a))


(defun uranus-eval (form)
  (ignore-errors (ura:toplevel-execute form T)))


(defcommand "Evaluate Uranus Region" (p)
     "Evaluate Uranus Region"
     "Evaluate Uranus Region"
  (let ((*readtable* uranus:uranus-readtable)
	(uranus:@printlevel 99)
        (*current-evaluator* 'uranus-eval)
        (*package* (find-package 'uranus)))
    (let ((buf (current-buffer)))
      (with-point-and-mark
        (message
         (with-output-to-string (*standard-output*)
           (with-input-from-string (in (points-to-string %point% %mark%))
             (loop :for xpr := (read in nil in)
                   :until (eq xpr in)
                   :do (uranus-eval xpr)
                       (terpri)))))))))


(bind-key "Evaluate Uranus Region" "Control-E" :mode "Uranus")


;;; *EOF*
