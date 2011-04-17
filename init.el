(unless (fboundp 'current-directory)
  (defun current-directory ()
    (cond (load-in-progress (file-name-directory load-file-name))
          (t default-directory)))
)

(add-to-list 'load-path (current-directory))


