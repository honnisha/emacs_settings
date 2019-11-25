(require 'bui)
(require 'request)

(setq frelansim-data (parsers-get-frelansim-date))

(defun links-buffer->entry (link_data)
  `(
    (name ,(nth 0 link_data))
    (desc ,(nth 1 link_data))
    (money ,(nth 1 link_data))
    (link ,(nth 2 link_data))
    ))

(defun links-get-entries ()
  (mapcar 'links-buffer->entry frelansim-data))

(bui-define-interface links list
  :link-name "*Links*"
  :get-entries-function 'links-get-entries
  :format '((name nil 30 t)
            (desc nil 80 t)
            (money nil 10 t)
            (link nil 30 t)
            )
  :sort-key '(name)
  )

(defun frelansim-links ()
  "Display a list of links."
  (interactive)
  (bui-get-display-entries 'links 'list))
