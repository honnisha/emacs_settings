Emacs settings
=======
My settings for emacs. Config for python, rust editing. Using lsp and virtualenvwrapper.</br>
</br>
Autor: Nikita Ivanov (Gangashman) ganagsh@gmail.com</br>

Installation
=======
Set two variables in your settings file: `settings_path` and `dropbox_path` and load `main.el`:
```lisp
(setq settings_path "/home/gagen/Projects/emacs_settings/")
(setq dropbox_path "/home/gagen/Dropbox/")

(load-file (concat settings_path "main.el"))
```

Screenshots
=======
<div align="center"><img src="https://github.com/gangashman/emacs_settings/blob/master/screenshots/1.png"/></div>

<div align="center"><img src="https://github.com/gangashman/emacs_settings/blob/master/screenshots/2.png"/></div>
