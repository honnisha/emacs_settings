(require 'bui)
(require 'request)

(setq link_data
      (list
      '((name "blah")
        (link "linkosik")
        (money "10 rubley ebat"))
      '((name "vano")
        (link "google.ru")
        (money "2 rubley ebat"))
      ))

(defun links-get-entries ()
  (request "http://httpbin.org/get"
 :params '(("key" . "value") ("key2" . "value2"))
 :parser 'json-read
 :success (cl-function
 (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default data)))))
  link_data
  )

(bui-define-interface links list
  :link-name "*Links*"
  :get-entries-function 'links-get-entries
  :format '((name nil 30 t)
            (money nil 10 t)
            (link nil 30 t)
            )
  :sort-key '(name)
  )

(defun frelansim-links ()
  "Display a list of links."
  (interactive)
  (bui-get-display-entries 'links 'list))
