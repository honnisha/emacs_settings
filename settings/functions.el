(defun eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

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

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message "File name: %s was added to clipboard" buffer-file-name)
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c f") 'show-file-name)

(defun lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (gethash "contents"))))

    (if (and contents (not (equal contents "")) )
        (pos-tip-show (lsp--render-on-hover-content contents t))
      (lsp--info "No content at point."))))


(defun describe-function-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (description (save-window-excursion
                        (describe-function thing)
                        (switch-to-buffer "*Help*")
                        (buffer-string))))
    (kill-buffer "*Help*")
    (pos-tip-show description))
  (pos-tip-cancel-timer))

;;     (pos-tip-show description
;;                :point (point)
;;                :around t
;;                :height 30
;;                :scroll-bar t
;;                :margin t)))

(defun lsp-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at
point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (gethash "contents"))))
    (if (and contents (not (equal contents "")))
        (pos-tip-show
         (string-trim-right (lsp--render-on-hover-content contents t)))
      (pos-tip-show "No content at point.")))
  (pos-tip-cancel-timer))
