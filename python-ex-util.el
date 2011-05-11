(require 'with-prefix)

(eval-when-compile (require 'cl))

(with-prefix 
    ((@ python-ex-util:)
     (% peu:))

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

  ;; utility
  (defun %fresh-buffer-and-is-created (buf &optional force-erase-p)
    (cond ((and (stringp buf) (not (get-buffer buf)))
           (values (get-buffer-create buf) t))
          (t
           (@let1 buf (if (bufferp buf) buf (get-buffer buf))
             (when force-erase-p
               (with-current-buffer buf
                 (erase-buffer)))
             (values buf force-erase-p)))))

  (defun %command-to-buffer-ansync (procname bufname cmd &optional force-reload-p call-back)
    (@let1 call-back (or call-back 'display-buffer)
      (multiple-value-bind (buf new-p)
          (%fresh-buffer-and-is-created bufname force-reload-p)
        (cond (new-p
               (set-process-sentinel
                (start-process-shell-command procname buf cmd)
                (@with-lexical-bindings (call-back)
                  (lambda (process status)
                    (funcall call-back (process-buffer process))))))
              (t (funcall call-back (get-buffer bufname))))
        buf)))
  

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

  (defun %all-parent-path-candidates (head words)
    "create all candidates: path -> (\"~/foo/bar/boo\" \"~/foo/bar\" \"~/foo\" \"~\")"
    (loop for word in words
          with acc = head
          unless (string-equal "" word)
          do (setq acc (concat acc "/" word))
          and collect acc into result
          finally return (nreverse (cons head result))))

  (defun %check-target-is-exist-in-path (path target)
    "checking targetted file is exist in path."
    (destructuring-bind (head . words) (split-string path "/")
      (@let1 candidates (%all-parent-path-candidates head words)
        (lexical-let ((target target))
          (find target candidates
                :test (lambda (target path)
                        (file-exists-p (concat path "/" target))))))))

  ;; (defun %current-rc ()
  ;;   (format "~/.%src" (file-name-nondirectory (getenv "SHELL"))))

  (defun %workon-path-maybe ()
    "if enable virtualenvwrapper then return workon home path"
    (getenv "WORKON_HOME"))

  (defun %current-env-maybe ()
    (or (@and-let* ((workon-path (%workon-path-maybe))
                    (rx (format "\\(%s/[^/]+\\)" (expand-file-name workon-path)))
                    ((string-match rx default-directory)))
          (match-string 1 default-directory))
        (let ((dir default-directory))
          (and dir (%check-target-is-exist-in-path dir "/bin/activate")))))

  (defun %command-format-with-current-env (cmd)
    "return format string branching venv or not"
    (or (@and-let* ((env-path (%current-env-maybe)))
          (format "(source %s/bin/activate && %s %%s)" env-path cmd))
        (format "%s %%s" cmd)))

  ;; default-functionを書き換えたいね。
  (defun %execute-command-with-current-env (cmd args &optional execute-function)
    (let ((fun (or execute-function
                   (lambda (&rest args) (message (apply 'shell-command-to-string args)))))
          (args-str (if (listp args) (mapconcat 'identity args " ") args)) 
          (cmd-format (%command-format-with-current-env cmd)))
      (funcall fun (format cmd-format args-str))))
  
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
  (defun @current-python-format () 
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
             (compile (format (@current-python-format) file))))))

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
                           (format (@current-python-format) (format "-c \"%s\"" script))))) ;; わかりにくい？
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

  
  ;; flymake
  (defvar @flymake-command "epylint")
  (defun @flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (cmd (format (%command-format-with-current-env @flymake-command) temp-file)))
      (list shell-file-name (list shell-command-switch cmd) default-directory)))
  ;;(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" @flymake-python-init))

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


  ;; completing from all module (too heavy)
  (defvar @previous-python nil)

  (defun @all-modules-to-buffer (&optional force-reload-p call-back)
    ;; return buffer
    (@let1 python (@current-python-format)
      (let* ((reload-p (or force-reload-p (not (string-equal @previous-python python))))
             (cmd (format python "-c 'import pydoc; import sys; pydoc.ModuleScanner().run(lambda path,modname,desc : sys.stdout.write(modname+\"\\n\"))' 2>/dev/null | sort -u"))
             (bufname "*python all-modules*"))

        (setq @previous-python python)
        (%command-to-buffer-ansync "python-ex-util:all-module"
                                   bufname cmd reload-p call-back))))

  (defun @all-modules-to-buffer-reload ()
    (message "collecting python module informations ...")
    (@all-modules-to-buffer 
     t (lambda (buf) (message "... done"))))

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
            (update . @all-modules-to-buffer-reload)
            ;;TODO: 便利なフローを考える
            (action . find-file)))

    (define-anything-type-attribute 'python-module
      '((action . (("find-file" . 
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
                   ("info-egg" .
                    (lambda (c)
                      (and-let* ((path (@module-name-to-egg-info c)))
                        (%find-file-safe path)))))))
      "Python module")

    (setq @anything-c-source-imported-modules
          '((name . "imported modules")
            (candidates . (lambda ()
                            (@collect-imported-modules-in-buffer  anything-current-buffer)))
            (update . @all-modules-to-buffer-reload)
            (type . python-module)))

    (setq @anything-c-source-all-modules
          '((name . "python all module")
            (candidates-in-buffer)
            (init . (lambda () 
                      (anything-candidate-buffer
                       (@all-modules-to-buffer 
                        nil 'identity))))
            (update .  @all-modules-to-buffer-reload)
            (type . python-module)))

    (defun @anything-ffap () (interactive)
      (@let1 sources (list @anything-c-source-imported-modules
                           anything-c-source-imenu
                           @anything-c-source-active-enves
                           @anything-c-source-all-modules)
        (anything-other-buffer sources " *ffap:python-ex:util*")))

    (setq @anything-c-source-insert-module
          '((name . "python module insert")
            (candidates-in-buffer)
            (init . (lambda () 
                      (anything-candidate-buffer
                       (@all-modules-to-buffer 
                        nil 'identity))))
            (update .  @all-modules-to-buffer-reload)
            (action . (("insert" . 
                        (lambda (c)
                          (save-excursion
                            (goto-char (point-min))
                            (while (looking-at "^#")
                              (forward-line 1))
                            (insert "import" c "\n"))))))))

    (defun @anything-insert-module () (interactive)
      (anything-other-buffer (list @anything-c-source-insert-module)
                             " *insert:python-ex:util*"))
    ))

;; test

;; 41:  (defun %shell-command-to-string* (command)
;; 45:  ;; (defun %first-line-in-string (str)
;; 48:  (defun* %find-file-safe (path &key open)
;; 53:  (defun %current-rc ()
;; 56:  (defun %workon-path-maybe ()
;; 60:  (defun %current-env-maybe ()
;; 66:  (defun %command-format-with-current-env (cmd)
;; 73:  (defun %execute-command-with-current-env (cmd args &optional execute-function)

(when (require 'chibi-test nil t)
  (with-shortcut-current-buffer
   (with-chibi-test*
    (section: "util"
              (section "%fresh-buffer-and-is-created"
                       (@let1 bufname "*f*"
                         (when (get-buffer bufname) (kill-buffer bufname))
                         
                         (test-true: "get fresh new buffer" 
                                     (multiple-value-bind (buf is-new-p)
                                         (%fresh-buffer-and-is-created bufname)
                                       (and (bufferp buf) (buffer-live-p buf)
                                            is-new-p)))
                         
                         (test-true: "get fresh but not new buffer"
                                     (multiple-value-bind (buf is-new-p)
                                         (%fresh-buffer-and-is-created bufname)
                                       (and (bufferp buf) (buffer-live-p buf)
                                            (null is-new-p))))

                         
                         (test-true: "get fresh buffer (of course, buffer-string is zero length)"
                                     (progn
                                       (with-current-buffer (get-buffer bufname)
                                         (insert "heheheh"))
                                       (multiple-value-bind (buf is-new-p)
                                           (%fresh-buffer-and-is-created bufname t)
                                         (and (bufferp buf) (buffer-live-p buf)
                                              (zerop (length (with-current-buffer buf (buffer-string))))))))))

              (section "%command-to-buffer-ansync"
                       (let ((bufname  "*buf*"))
                         (test: "async ???" ""
                                (@with-lexical-bindings (bufname)
                                  (%command-to-buffer-ansync 
                                   "ls" bufname "ls" t 
                                   (lambda (b)
                                     (test-not: "async !!!" 0 (with-current-buffer b (buffer-string)))))
                                  (with-current-buffer (get-buffer bufname)
                                    (buffer-string))))))

              (section "%tmp-file"
                       (test-not "tmp-file generate uniq filename" (%tmp-file) (%tmp-file)))))))


(provide 'python-ex-util)
