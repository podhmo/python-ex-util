(require 'with-prefix)

(eval-when-compile (require 'cl))

(with-prefix 
    ((@ python-ex-util:)
     (util. peu:))

  ;; utility
  (defun util.tmp-file (&optional file)
    "return uniq name in tmp-dirctory"
    (cond ((null file) (util.tmp-file (format "peu:%s.py" (gensym))))
          (t (let* ((begin-with-slash-p  (char-equal ?/ (aref file 0)))
                    (file* (if begin-with-slash-p (substring-no-properties file 1 (length file)) file)))
               (concat "/tmp/" file*)))))

  (defun util.current-rc ()
    (format "~/.%src" (file-name-nondirectory (getenv "SHELL"))))

  (defun util.workon-path-maybe ()
    "if enable virtualenvwrapper then return workon home path"
    (getenv "WORKON_HOME"))

  (defun util.current-env-maybe ()
    (@and-let* ((workon-path (util.workon-path-maybe))
                (rx (format "%s/\\([^/]+\\)" (expand-file-name workon-path)))
                ((string-match rx default-directory)))
      (match-string 1 default-directory)))

  (defun util.command-format-with-current-env (cmd)
    "return format string branching venv or not"
    (or (@and-let* ((env (util.current-env-maybe)))
          (format "(source %s && workon %s && %s %%s)" (util.current-rc) env cmd))
        (format "%s %%s" cmd)))

  ;; default-functionを書き換えたいね。
  (defun util.execute-command-with-current-env (cmd args &optional execute-function)
    (let ((fun (or execute-function
                   (lambda (&rest args) (message (apply 'shell-command-to-string args)))))
          (args-str (if (listp args) (mapconcat 'identity args " ") args)) 
          (cmd-format (util.command-format-with-current-env cmd)))
      (funcall fun (format cmd-format args-str))))

  (defmacro @with-lexical-bindings (syms &rest body)
    (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
      (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
  (put '@with-lexical-bindings 'lisp-indent-function 1)

  (defmacro @with-gensyms (syms &rest body)
    (let ((bindings (mapcar (lambda (x) (\` ((\, x) '(\, (gensym))))) syms)))
      (\` (let ((\,@ bindings)) (\,@ body)))))
  (put '@with-gensyms 'lisp-indent-function 1)
  
  (defmacro @rlet1 (var val &rest body)
    "imported from gauche"
    (\` (@let1 (\, var) (\, val) (\,@ body) (\, var))))
  (put '@rlet1 'lisp-indent-function 2)

  (defmacro @let1 (var val &rest body)
    "imported from gauche"
    (\` (let (((\, var) (\, val))) (\,@ body))))
  (put '@let1 'lisp-indent-function 2)

  (defmacro @and-let* (bindings &rest body)
    "imported from srfi-2"
    (reduce (function
             (lambda (binding r)
               (let ((head (car binding)))
                 (cond ((and (atom head) (symbolp head))
                        (\` (let ((\, binding)) (when (\, head) (\, r)))))
                       ((listp head)
                        (\` (when (\, head) (\, r))))
                       (t (error "and-let*: invalid head %s" head))))))
            bindings
            :from-end
            t
            :initial-value
            (\` (progn (\,@ body)))))
  (put '@and-let* 'lisp-indent-function 1)

  (defmacro @alambda (args &rest body)
    "Anaphoric lambda. enable to self recursion using `self' anaphorar"
    (\` (labels ((self (\, args) (\,@ body))) (function self))))
  (put '@alambda 'lisp-indent-function 1)

  (defmacro @aand (&rest args)
    "Anaphoric and. anaphorar is `it'"
    (cond ((null args)
           t)
          ((null (cdr args))
           (car args))
          (t (\` (@aif (\, (car args)) (@aand (\,@ (cdr args))))))))

  (defmacro @aif (test-form then-form &rest else-forms)
    "Anaphoric if. Temporary variable `it' is the result of test-form."
    (\` (let ((it (\, test-form))) (if it (\, then-form) (\,@ else-forms)))))
  (put '@aif 'lisp-indent-function 2)
  
   ;;; current-python
  (defun @current-python () 
    (util.command-format-with-current-env "python"))

  ;; interactive command using `python-ex-util:current-python'
  (defun @describe-current-python () (interactive)
    (util.execute-command-with-current-env 
     "python" "-c 'import os; os.system(\"which python\")'"))

  (defvar @tmp-file-auto-cleaning-p nil
    "this variable is true, then, auto cleaning after using tmp-file(this occured is working on non-file buffer),")

  (defun @eval-buffer-with-current-python (&optional file) (interactive "P")
    (@let1 file (or file (buffer-file-name))
      (cond ((null file)
             (let ((bufstring (buffer-string))
                   (file (util.tmp-file)))
               (with-temp-file file
                 (insert bufstring)
                 (@eval-buffer-with-current-python file))
               (when @tmp-file-auto-cleaning-p (delete-file file))))
            (t
             (compile (format (@current-python) file))))))

    ;;; ffap from import module sentence
  (defvar @module-tokens-regexp "[^ .]+\\(\\.[^ .]+\\)*")

  (defun @module-tokens-in-current-line ()
    "[maybe] return match object or nil"
    (save-excursion
      (let ((rx (format "^[ \t]*\\(from +\\(%s\\) +import +[^ ]+\\|import +\\(%s\\)\\)"
                        @module-tokens-regexp
                        @module-tokens-regexp
                        )))
        (goto-char (point-at-bol))
        (and (re-search-forward rx  (point-at-eol) t 1)
             (or (match-string-no-properties 4)
                 (match-string-no-properties 2))))))


  (defun @library-path-list ()
    (let* ((script "import sys; D=[d for d in sys.path if not 'bin' in d]; print ','.join(D)")
           (sys-paths-str (shell-command-to-string
                           (format (@current-python) (format "-c \"%s\"" script))))) ;; わかりにくい？
      (cdr (split-string sys-paths-str ","))))

  (defun @module-name-from-path (module &optional force-reload-p)
    (@let1 path (replace-regexp-in-string "\\." "/" module)
      (loop for dir in (@library-path-list)
            for d = (concat dir "/" path)
            if (file-exists-p d)
            return d
            else
            for file = (concat d ".py")
            when (file-exists-p file)
            return file)))

  (defun @current-library-path ()
    "[maybe] return current library path from import sentence"
    (@and-let* ((module (@module-tokens-in-current-line)))
      (@module-name-from-path module)))

  (defun @ffap/import-sentence (other-frame-p) (interactive "P")
    "ffap for import sentence"
    (@and-let* ((path (@current-library-path)))
      (cond (other-frame-p (find-file-other-frame path))
            (t (find-file path)))))

  ;; venvs
  (defun @active-venv-list (&optional fullpath-p)
    "from virtualenvwrapper_show_workon_options"
    (@and-let* ((workon-path (util.workon-path-maybe)))
      (loop for file in (directory-files workon-path t)
            when (and (file-directory-p file) (file-exists-p (concat file "/bin/activate")))
            collect (if fullpath-p file (file-name-nondirectory file)))))

  )

(provide 'python-ex-util)
