(require 'with-prefix)

(eval-when-compile (require 'cl))

(with-prefix 
    ((@ python-ex-util:)
     (% peu:))

  ;; utility
  (defun %tmp-file (&optional file)
    "return uniq name in tmp-dirctory"
    (cond ((null file) (%tmp-file (format "peu:%s.py" (gensym))))
          (t (let* ((begin-with-slash-p  (char-equal ?/ (aref file 0)))
                    (file* (if begin-with-slash-p (substring-no-properties file 1 (length file)) file)))
               (concat "/tmp/" file*)))))

  (defun %shell-command-to-string* (command)
    (let ((r (shell-command-to-string command)))
      (substring-no-properties r 0 (- (length r) 1))))

  ;; (defun %first-line-in-string (str)
  ;;   (car (split-string str "\n")))

  (defun* %find-file-safe (path &key open)
    (and (file-exists-p path)
         (file-readable-p path)
         (funcall (or open 'find-file) path)))

  (defun %current-rc ()
    (format "~/.%src" (file-name-nondirectory (getenv "SHELL"))))

  (defun %workon-path-maybe ()
    "if enable virtualenvwrapper then return workon home path"
    (getenv "WORKON_HOME"))

  (defun %current-env-maybe ()
    (@and-let* ((workon-path (%workon-path-maybe))
                (rx (format "%s/\\([^/]+\\)" (expand-file-name workon-path)))
                ((string-match rx default-directory)))
      (match-string 1 default-directory)))

  (defun %command-format-with-current-env (cmd)
    "return format string branching venv or not"
    (or (@and-let* ((env (%current-env-maybe)))
          (format "(source %s && workon %s && %s %%s)" (%current-rc) env cmd))
        (format "%s %%s" cmd)))

  ;; default-functionを書き換えたいね。
  (defun %execute-command-with-current-env (cmd args &optional execute-function)
    (let ((fun (or execute-function
                   (lambda (&rest args) (message (apply 'shell-command-to-string args)))))
          (args-str (if (listp args) (mapconcat 'identity args " ") args)) 
          (cmd-format (%command-format-with-current-env cmd)))
      (funcall fun (format cmd-format args-str))))

  ;; macros
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
  

  (defvar @standard-doc-url-base "http://docs.python.org/library")

  (defun @module-name-to-file-path (module &optional force-reload-p) ;;force-reload is not implemented
    "python module -> file path"
    (@let1 path (replace-regexp-in-string "\\." "/" module)
      (loop for dir in (@library-path-list)
            for d = (concat dir "/" path)
            if (file-exists-p d)
            return d
            else
            for file = (concat d ".py")
            when (file-exists-p file)
            return file)))

  (defun @module-name-to-egg-info (module &optional force-reload-p) ;;force-reload is not implemented
    (let* ((module-top (car (split-string module "\\.")))
           (info-rx (format "\\(%s\\|%s\\)-.*egg-info$" (capitalize module-top) module-top)))
      (loop for dir in (@library-path-list)
            when (and (file-exists-p dir) (file-directory-p dir))
            for candidate = (directory-files dir t info-rx t)
            when candidate return (car candidate))))

  (defun @module-name-to-web-page (module &optional force-reload-p) ;;force-reload is not implemented
    (or (@and-let* ((egg-info (@module-name-to-egg-info module force-reload-p))
                    (file (format "%s/PKG-INFO" egg-info)))
          (message egg-info)
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (re-search-forward "Home-page: *\\(.+\\)" nil t 1)
            (match-string 1)))
        (format "%s/%s.html" @standard-doc-url-base module)))

   ;;; current-python
  (defun @current-python () 
    (%command-format-with-current-env "python"))

  ;; interactive command using `python-ex-util:current-python'
  (defun @describe-current-python () (interactive)
    (%execute-command-with-current-env 
     "python" "-c 'import os; os.system(\"which python\")'"))

  (defvar @tmp-file-auto-cleaning-p nil
    "this variable is true, then, auto cleaning after using tmp-file(this occured is working on non-file buffer),")

  (defun @eval-buffer-with-current-python (&optional file) (interactive "P")
    (@let1 file (or file (buffer-file-name))
      (cond ((null file)
             (let ((bufstring (buffer-string))
                   (file (%tmp-file)))
               (with-temp-file file
                 (insert bufstring)
                 (@eval-buffer-with-current-python file))
               (when @tmp-file-auto-cleaning-p (delete-file file))))
            (t
             (compile (format (@current-python) file))))))

    ;;; ffap from import module sentence
  (setq @module-tokens-regexp "[^\n .]+\\(\\.[^\n .]+\\)*")
  (setq @module-import-sentence-regexp 
        (format "^[\n \t]*\\(from +\\(%s\\) +import +[^ \n]+\\|import +\\(%s\\)\\)"
                @module-tokens-regexp
                @module-tokens-regexp))

  (defun @find-module-tokens-maybe (&optional beg end)
    "[maybe] find module import sentence, beg and end are optional (default end value is `point-at-eol')"
    (when beg
      (goto-char beg))
    (let ((end (or end (point-at-eol))))
      (and (re-search-forward @module-import-sentence-regexp end t 1)
           (or (match-string-no-properties 4)
               (match-string-no-properties 2)))))

  (defun @module-tokens-in-current-line ()
    "[maybe] return match object or nil"
    (save-excursion
      (@find-module-tokens-maybe (point-at-bol) (point-at-eol))))


  (defun @library-path-list ()
    (let* ((script "import sys; D=[d for d in sys.path if not 'bin' in d]; print ','.join(D)")
           (sys-paths-str (%shell-command-to-string*
                           (format (@current-python) (format "-c \"%s\"" script))))) ;; わかりにくい？
      (cdr (split-string sys-paths-str ","))))

  (defun @current-library-path ()
    "[maybe] return current library path from import sentence"
    (@and-let* ((module (@module-tokens-in-current-line)))
      (@module-name-to-file-path module)))

  (defun @ffap/import-sentence (other-frame-p) (interactive "P")
    "ffap for import sentence"
    (@and-let* ((path (@current-library-path)))
      (cond (other-frame-p (find-file-other-frame path))
            (t (find-file path)))))

  ;; venvs
  (defun @active-venv-list (&optional fullpath-p)
    "from virtualenvwrapper_show_workon_options"
    (@and-let* ((workon-path (%workon-path-maybe)))
      (loop for file in (directory-files workon-path t)
            when (and (file-directory-p file) (file-exists-p (concat file "/bin/activate")))
            collect (if fullpath-p file (file-name-nondirectory file)))))

  (defun @find-venv () (interactive)
    (find-file
     (completing-read "venv: " (@active-venv-list t))))

  ;; anything
  (when (or (fboundp 'anything) (require 'anything nil t))

    (defun @collect-imported-modules-in-buffer (&optional buf)
      (@let1 buf (or buf (current-buffer))
        (with-current-buffer buf
          (goto-char (point-min))
          (@let1 end (point-max)
            (loop while t
                  for token = (@find-module-tokens-maybe nil end)
                  if token
                  collect token into modules
                  else
                  return (delete-duplicates modules :test 'string-equal))))))

    (setq @anything-c-source-active-enves
          '((name . "active virturl envs")
            (candidates . (lambda () (@active-venv-list t)))
            ;;TODO: 便利なフローを考える
            (action . find-file)))

    (setq @anything-c-source-imported-modules
          '((name . "imported modules")
            (candidates . (lambda ()
                            (@collect-imported-modules-in-buffer  anything-current-buffer)))
            ;;TODO: actionを追加
            (action . (("find-file" . 
                        (lambda (c)
                          (and-let* ((path (@module-name-to-file-path c)))
                            (%find-file-safe path))))
                       ("find-file-other-frame" .
                        (lambda (c)
                          (and-let* ((path (@module-name-to-file-path c)))
                            (%find-file-safe path :open 'find-file-other-frame))))
                       ("web-page" .
                        (lambda (c)
                          (and-let* ((url (@module-name-to-web-page c)))
                            (browse-url-generic url))))
                       ))))

    (defun @anything-ffap () (interactive)
      (@let1 sources (list @anything-c-source-imported-modules
                           @anything-c-source-active-enves)
        (anything-other-buffer sources " *ffap:python-ex:util*")))
    )
  )




(provide 'python-ex-util)
