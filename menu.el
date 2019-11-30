(require 'bui)

(defun links-buffer->entry (link_data)
  `(
    (title ,(nth 0 link_data))
    (responses ,(nth 2 link_data))
    (views ,(nth 3 link_data))
    (time ,(nth 4 link_data))
    (link ,(nth 1 link_data))
    (money ,(nth 5 link_data))
    ))

(defun links-get-entries ()
  (mapcar 'links-buffer->entry (parserslib-get-frelansim-env)))

(bui-define-interface links list
  :link-name "*Links*"
  :get-entries-function 'links-get-entries
  :format '((title nil 60 t)
            (responses nil 3 t)
            (views nil 3 t)
            (time nil 20 t)
            (money nil 35 t)
            )
  )

(defun frelansim-links ()
  "Display a list of links."
  (interactive)
  (bui-get-display-entries 'links 'list))
