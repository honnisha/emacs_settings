(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; How can I prevent a command from using specific windows?
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))
(global-set-key (kbd "C-c l") 'toggle-window-dedicated)


(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
	(set-buffer buffer)
	(when (equal major-mode 'dired-mode)
	  (setq count (1+ count))
	  (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))
(global-set-key (kbd "C-x d") 'kill-all-dired-buffers)


(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

(reverse-input-method 'russian-typewriter)

(modify-coding-system-alist 'file "\\.txt\\'" 'windows-1251)

 ;; Eval and Replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x e") 'eval-and-replace)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
  If there's no region, the current line will be duplicated. However, if
  there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message "File name: %s was added to clipboard" buffer-file-name)
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c f") 'show-file-name)


(require 'shell)

(defun shell-get-buffer-create (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
   (prog1
       (read-buffer "Shell buffer: "
        ;; If the current buffer is an inactive
        ;; shell buffer, use it as the default.
        (if (and (eq major-mode 'shell-mode)
           (null (get-buffer-process (current-buffer))))
            (buffer-name)
          (generate-new-buffer-name "*shell*")))
     (if (file-remote-p default-directory)
         ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
         (setq default-directory
         (expand-file-name
          (read-directory-name
           "Default directory: " default-directory default-directory
           t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
     (file-remote-p default-directory)
     (null explicit-shell-file-name)
     (null (getenv "ESHELL")))
      (with-current-buffer buffer
  (set (make-local-variable 'explicit-shell-file-name)
       (file-remote-p
        (expand-file-name
         (read-file-name
    "Remote shell path: " default-directory shell-file-name
    t shell-file-name))
        'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (with-current-buffer buffer
    (unless (comint-check-proc buffer)
      (let* ((prog (or explicit-shell-file-name
           (getenv "ESHELL") shell-file-name))
       (name (file-name-nondirectory prog))
       (startfile (concat "~/.emacs_" name))
       (xargs-name (intern-soft (concat "explicit-" name "-args"))))
        (unless (file-exists-p startfile)
    (setq startfile (concat user-emacs-directory "init_" name ".sh")))
        (apply 'make-comint-in-buffer "shell" buffer prog
         (if (file-exists-p startfile) startfile)
         (if (and xargs-name (boundp xargs-name))
       (symbol-value xargs-name)
           '("-i")))
        (shell-mode))))
  buffer)

(defun my-display-buffer (buffer alist direction &optional size pixelwise)
"BUFFER:  The buffer that will be displayed.
ALIST:  See the doc-string of `display-buffer' for more information.
DIRECTION:  Must use one of these symbols:  'left 'right 'below 'above
SIZE:  See the doc-string for `split-window'.
PIXELWISE:  See the doc-string for `split-window'.
There are three possibilities:
-  (1) If a window on the frame already displays the target buffer,
then just reuse the same window.
-  (2) If there is already a window in the specified direction in relation
to the selected window, then display the target buffer in said window.
-  (3) If there is no window in the specified direction, then create one
in that direction and display the target buffer in said window."
  (let ((window
          (cond
            ((get-buffer-window buffer (selected-frame)))
            ((window-in-direction direction))
            (t
              (split-window (selected-window) size direction pixelwise)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))


(defun shell (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          ;; If the current buffer is an inactive
                          ;; shell buffer, use it as the default.
                          (if (and (eq major-mode 'shell-mode)
                                   (null (get-buffer-process (current-buffer))))
                              (buffer-name)
                            (generate-new-buffer-name "*shell*")))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
           (file-remote-p default-directory)
           (null explicit-shell-file-name)
           (null (getenv "ESHELL")))
      (with-current-buffer buffer
        (set (make-local-variable 'explicit-shell-file-name)
             (file-remote-p
              (expand-file-name
               (read-file-name
                "Remote shell path: " default-directory shell-file-name
                t shell-file-name))
              'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (switch-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
                     (getenv "ESHELL") shell-file-name))
           (name (file-name-nondirectory prog))
           (startfile (concat "~/.emacs_" name))
           (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
        (setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
             (if (file-exists-p startfile) startfile)
             (if (and xargs-name (boundp xargs-name))
                 (symbol-value xargs-name)
               '("-i")))
      (shell-mode)))
  buffer)

(defun jedi:show-doc-in-tip ()
  "Show the documentation of the object at point."
  (interactive)
  (deferred:nextc (jedi:call-deferred 'get_definition)
    (lambda (reply)
      (with-current-buffer (get-buffer-create jedi:doc-buffer-name)
        (cl-loop with has-doc = nil
                 with first = t
                 with inhibit-read-only = t
                 initially (erase-buffer)
                 for def in reply
                 do (cl-destructuring-bind
                        (&key doc desc_with_module &allow-other-keys)
                        def
                      (unless (or (null doc) (equal doc ""))
                        (if first
                            (setq first nil)
                          (insert "\n\n---\n\n"))
                        (insert "Docstring for " desc_with_module "\n" doc)
			; (pos-tip-show doc)(mouse-pixel-position)
			(pos-tip-show doc)
			(pos-tip-cancel-timer)
                        (setq has-doc t)))
                 finally do
                 (if (not has-doc)
		     (pos-tip-show "Document not found.")
		   (progn
		     ;;(goto-char (point-min))
		     ;;(when (fboundp jedi:doc-mode)
		     ;;  (funcall jedi:doc-mode))
		     ;;(run-hooks 'jedi:doc-hook)
		     ;;(funcall jedi:doc-display-buffer (current-buffer))
		     )
		   ))))))
