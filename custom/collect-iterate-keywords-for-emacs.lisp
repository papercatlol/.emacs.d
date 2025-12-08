(in-package :iterate)


(defvar *iterate-keywords-emacs* nil)

(defun collect-iterate-keywords-for-emacs (index-or-clause)
  (if (index? index-or-clause)
      (iter (for entry :in (cdr index-or-clause))
        (collect-iterate-keywords-for-emacs (cdr entry)))
    (let* ((args (clause-info-keywords index-or-clause))
           (keywords (remove-if-not #'keywordp args)))
      (when keywords
        (push (cons (car args) keywords) *iterate-keywords-emacs*)))))


(collect-iterate-keywords-for-emacs *clause-info-index*)

*iterate-keywords-emacs*
