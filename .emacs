(setq settings_path "/home/gagen/Projects/emacs_settings/")

(package-initialize)

(load-file (concat settings_path "main.el"))

(global-set-key (kbd "C-x C-n") (lambda() (interactive)(find-file "/home/gagen/Dropbox/Новый текстовый документ.txt")))
