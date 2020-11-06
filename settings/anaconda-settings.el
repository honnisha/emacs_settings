
(defun anaconda-mode-show-doc-callback (result)
  "Process view doc RESULT."
  (if (> (length result) 0)
      (if (and anaconda-mode-use-posframe-show-doc
               (require 'posframe nil 'noerror)
               (posframe-workable-p))
          (anaconda-mode-documentation-posframe-view result)
        (anaconda-mode-documentation-view result))
    (pos-tip-show "No documentation available")))

(defface anaconda-pos-tip-help-header
  '((t
     :foreground "yellow green"
     :bold t))
  "Face for anaconda doc tooltip header.")

(defun anaconda-mode-documentation-view (result)
  "Show documentation view for rpc RESULT, and return buffer."
  (let ((pos-tip-background-color "gray25"))
    (pos-tip-show-no-propertize
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t))
         (mapc
          (lambda (it)
            (insert (propertize (aref it 0) 'face 'anaconda-pos-tip-help-header))
            (insert "\n")
            (insert (s-trim-right (aref it 1)))
            (insert "\n\n"))
          result)
         (buffer-string))))
    )
  (pos-tip-cancel-timer)
  )
