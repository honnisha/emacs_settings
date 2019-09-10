(setq settings_path "/home/gagen/Projects/emacs_settings/")
(setq dropbox_path "/home/gagen/Dropbox/")

(package-initialize)

(load-file (concat settings_path "main.el"))

(global-set-key (kbd "C-x C-n") (lambda() (interactive)(find-file (concat dropbox_path "Новый текстовый документ.txt"))))
