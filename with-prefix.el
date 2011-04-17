;; (setq debug-on-error t)
(eval-when-compile (require 'cl))

;; internal variables
(defvar with-prefix-buffer-prefix-alist nil
  "this variable is internal variable. don't change value.")


(defmacro with-prefix (target replacement &rest body)
  "with-prefix is pseudo-name-space(but roughly implement)

use this::
(with-prefix @ with-prefix: body ...)"

  (declare (indent 0))
  (let* ((target-str (if (symbolp target) (symbol-name target) target))
         (target-rx (format "^%s" target-str))
         (replacement-str (format "%s" replacement)))

    `(progn
       ;; add a relation  for describe-function
       (add-to-list 'with-prefix-buffer-prefix-alist
                    (list ,(buffer-name (current-buffer)) ,replacement-str ,target-str))
       ;; replace
       ,@(wp:tree-map-safe
          (lambda (sym) 
            (if (symbolp sym)
                (intern
                 (replace-regexp-in-string ;;slack off
                  target-rx replacement-str
                  (symbol-name sym)))
                sym))
          body))))

;; a advice for finding function location
(defadvice find-function-search-for-symbol
  (after with-prefix-force-find last (symbol type library) activate)
  (destructuring-bind (buf . pt) ad-return-value

    ;; when a function definition point is not found
    (unless pt
      (let ((pat-rep-values-maybe 
             (assoc-default (buffer-name buf) with-prefix-buffer-prefix-alist)))

        ;; and a pattern-replacement relation is found
        (when pat-rep-values-maybe
          (destructuring-bind (pat rep) pat-rep-values-maybe
            (let ((pat* (format "^%s" pat)))

              ;; force finding the function definition
              (with-current-buffer buf
                (goto-char (point-min))
                (and (search-forward (replace-regexp-in-string pat* rep (symbol-name symbol)) nil t)
                     (setq ad-return-value (cons buf (point))))))))))))

;; utility
(defun wp:mapcar-safe (fn maybe-list)
  "mapcar enable to iterate maybe-list (include dot-list)"
  (let ((r (list)) (xs maybe-list))
    (condition-case e
        (progn
          (while (not (null xs))
            (push (funcall fn (car xs)) r)
            (setq xs (cdr xs)))
          (nreverse r))
      (error (rlet1 r* (nreverse r)
               (setcdr (last r*) (funcall fn xs)))))))

(defun wp:tree-map-safe (fn tree)
  "`wp:mapcar-safe' recursive version"
  (lexical-let ((fn fn))
    (labels ((rec (tree)
                  (wp:mapcar-safe #'(lambda (x) (if (listp x) (rec x) (funcall fn x)))
                                  tree)))
      (rec tree))))


;; ;; for debugging
;; ;; (with-prefix 
;; ;;   @ with-prefix:
;; ;;     (defun @odd? (x) 
;; ;;       "check a received argument is odd number or not"
;; ;;       (if (<= x 0) nil (@even? (- x 1))))
;; ;;     (defun @even? (x)
;; ;;       (if (<= x 0) t (@odd? (- x 1))))

;; ;;     (print (list '@odd? 10 (@odd? 10)))
;; ;;     (print (list '@odd? 9 (funcall '@odd? 9)))
;; ;;     (print (list '@even? 10 (apply '@even? '(10)))))


;; ;; (add-to-list 'load-path default-directory)
;; ;; (find-function-search-for-symbol 'with-prefix:odd\? nil "with-prefix.el")
;; ;; (find-function-search-for-symbol 'with-prefix nil "with-prefix.el")

(provide 'with-prefix)