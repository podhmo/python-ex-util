(unless (fboundp 'current-directory)
  (defun current-directory ()
    (cond (load-in-progress (file-name-directory load-file-name))
          (t default-directory)))
)

(add-to-list 'load-path (current-directory))
(require 'python-ex-util)

(defun peu:init ()
  (let ((kmap (current-local-map)))
    (define-key kmap "\C-c@" 'python-ex-util:eval-buffer-with-current-python)
    (cond ((fboundp 'anything)
           (define-key kmap "\C-c\C-f" 'python-ex-util:anything-ffap))
          (t 
           (define-key kmap "\C-c\C-f" 'python-ex-util:ffap/import-sentence)))))

(defmacro peu:dispatch (py-mode python-mode)
  `(if (boundp 'py-mode-hook) ',py-mode ',python-mode))

(add-hook (peu:dispatch py-mode-hook python-mode-hook) 'peu:init)



