(require 'bui)

(setq link_data
      '(("name" . "vano")
        ("link" . "link")
        ("money" . "10 rubley ebat")))

(defun links-link->entry (link)
  (with-current-buffer link
    `((name . ,(cdr (assoc "name" link)))
      (link . ,(cdr (assoc "link" link)))
      )))

(defun links-get-entries ()
  link_data)

(bui-define-interface links list
  :link-name "*Links*"
  :get-entries-function 'links-get-entries
  :format '((name nil 30 t)
            (link nil 25 t))
  :sort-key '(name))

(defun frelansim-links ()
  "Display a list of links."
  (interactive)
  (bui-get-display-entries 'links 'list))
