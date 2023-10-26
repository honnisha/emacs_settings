;; init.el
;; (setq settings_path "/home/honnisha/Projects/emacs_settings/")
;; (setq dropbox_path "/home/honnisha/Dropbox/")
;; (setq doom-font (font-spec :family "Hack" :size 11 :weight 'light))
;; (load! (concat settings_path "init.el"))

;; config.el
;; (load! (concat settings_path "config.el"))

;; packages.el
;; (load-file (concat settings_path "packages.el"))

;; ~/.bashrc
;; source "$HOME/Dropbox/aliases"
;; VIRTUALENVWRAPPER_PYTHON=$(which python3)
;; source ~/.local/bin/virtualenvwrapper.sh

;; (setq doom-theme 'doom-wilmersdorf)
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-nord)

;; cd ~/Projects/yay-git
;; makepkg -si

;; yay -S docker-compose docker
;; sudo systemctl enable docker
;; sudo groupadd docker
;; sudo usermod -aG docker $USER
;; sudo chmod 666 /var/run/docker.sock

;; yay -S ntfs-3g os-prober
;; sudo grub-mkconfig -o /boot/grub/grub.cfg

(global-set-key (kbd "C-x C-n") (lambda() (interactive)(find-file (concat dropbox_path "text.org"))))

;; Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

(recentf-mode nil)

(set-fontset-font "fontset-default" 'cyrillic "Hack")
(set-fontset-font "fontset-default" 'greek "Hack")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(toggle-frame-maximized)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat doom-cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat doom-cache-dir "backup/"))))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

(setq x-gtk-use-system-tooltips t)

(setq pos-tip-background-color "gray20")
(setq pos-tip-saved-max-width-height 100)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; How to overwrite text by yank in Emacs?
(delete-selection-mode 1)

(display-time-mode 1)

;; If you enable winner-mode, you get something akin to a stack-based
;; undo/redo functionality for all your window configuration changes.
;; By default, C-c <left> gets bound to winner-undo, while C-c <right> performs winner-redo.
(winner-mode 1)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;; From Pragmatic Emacs a more concise way to kill the buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setq bookmark-default-file (concat dropbox_path "emacs-bookmarks"))
(global-set-key (kbd "<f2>") 'bookmark-jump)
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'bookmark-jump)
(global-set-key (kbd "<f3>") 'bookmark-set)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

;; (global-set-key (kbd "<C-M-tab>") 'next-buffer)
;; (global-set-key (kbd "<C-M-iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "<C-tab>") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-iso-lefttab>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<C-S-tab>") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)

;; Hide/Show
;; package-list-packages
(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l h") `hide-subtree)
(global-set-key (kbd "C-x l s") `show-subtree)
(global-set-key (kbd "C-x l a") `show-all)

;; Open buffers list in the same frame
;; (global-set-key "\C-x\C-b" 'buffer-menu)
(global-unset-key "\C-x\C-b")
(global-set-key (kbd "C-x y") `repeat-complex-command)

(global-set-key (kbd "C-x C-d") (lambda() (interactive)(find-file (concat settings_path "config.el"))))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-h") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-c r") `revert-buffer)
(global-set-key (kbd "C-r") `replace-string)

(global-set-key (kbd "C-x b") 'ibuffer-list-buffers)

(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-j"))

(global-set-key (kbd "C-.") `undo)

(defun next-with-center (lines)
  "Move LINES lines and center."
  (next-line lines)
  (recenter))
(global-set-key (kbd "C-M-n")
                (lambda () (interactive) (next-with-center 5)))
(defun previous-with-center (lines)
  "Move LINES lines and center."
  (previous-line lines)
  (recenter))
(map! :leader
      "C-M-p" #'(lambda () (interactive) (previous-with-center 5)))
(global-set-key (kbd "C-M-p")
                (lambda () (interactive) (previous-with-center 5)))

;; Windows/frames
(global-set-key (kbd "<C-S-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'shrink-window)

(use-package! whitespace
  :config
  ; (global-whitespace-mode 1)
  (global-set-key (kbd "C-c h") `whitespace-mode)
  (setq whitespace-style
        (quote (
                face
                trailing
                tabs
                empty
                indention
                spaces
                space-mark
                space-after-tab
                space-before-tab
                tab-mark
                )))
  (setq whitespace-line-column 1000)
  (set-face-attribute 'whitespace-line nil
                    :foreground nil
                    :background "gray10"
                    :weight 'bold)

  (add-hook 'web-mode-hook #'whitespace-mode)
  (add-hook 'css-mode-hook #'whitespace-mode)
  (add-hook 'emacs-lisp-mode-hook #'whitespace-mode)
  (add-hook 'yaml-mode-hook #'whitespace-mode)
  )

(use-package all-the-icons
  :config
  )

;; yay -S cmake-git
(use-package! vterm
  :config
  (add-hook 'vterm-mode  'vterm-copy-mode)
  (global-set-key (kbd "C-x m") '+vterm/here)
  (define-key vterm-mode-map (kbd "C-o") 'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-o") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-y") 'vterm-yank)
  (define-key vterm-mode-map (kbd "C-h") 'vterm-send-backspace)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
  (define-key vterm-mode-map (kbd "M-h") 'vterm-send-meta-backspace)
  (define-key vterm-mode-map (kbd "M-w") 'kill-ring-save)
  (define-key vterm-mode-map (kbd "M-m") 'vterm-send-C-a)
  (define-key vterm-mode-map (kbd "C-c l") 'vterm-clear)
  )

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(use-package! back-button
  :config
  (back-button-mode 1)
  ;; Back navigation
  (global-set-key (kbd "M-n") 'back-button-local-forward)
  (global-set-key (kbd "M-p") 'back-button-local-backward)
  ;; (global-set-key (kbd "<M-right>") 'back-button-global-forward)
  ;; (global-set-key (kbd "<M-left>") 'back-button-global-backward)
  (setq back-button-never-push-mark nil)
  )

(use-package! vertico
  :init
  (vertico-mode)
  :config
  (setq completion-styles '(basic substring partial-completion flex))
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  (setq vertico-resize nil)
  (setq vertico-count 30)
  (setq vertico-cycle t)
  )

(use-package! all-the-icons-completion
  :init
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  )

(use-package! marginalia
  :init
  (marginalia-mode)
  )

(use-package! consult
  :config
  (setq completion-styles '(substring basic))

  (global-set-key (kbd "C-S-SPC") 'consult-buffer)
  (global-set-key (kbd "C-x f") 'consult-find)
  ;; (global-set-key (kbd "C-M-s") 'consult-grep)
  (global-set-key (kbd "C-M-s") 'consult-git-grep)
  (global-set-key (kbd "C-s") 'consult-line)

  (after! consult
    (defadvice! org-show-entry-consult-a (fn &rest args)
      :around #'consult-line
      :around #'consult-org-heading
      :around #'consult--grep
      (when-let ((pos (apply fn args)))
        (org-fold-reveal '(4)))))

  (after! consult
    (defadvice! org-show-entry-consult-a (fn &rest args)
      :around #'consult-line
      :around #'consult-org-heading
      :around #'consult--grep
      (when-let ((pos (apply fn args)))
        (progn (org-fold-reveal '(4)) (org-fold-show-entry)))))

  (consult-customize
   ;; Disable preview for `consult-theme' completely.
   consult-theme :preview-key nil
   ;; Set preview for `consult-buffer' to key `M-.'
   consult-buffer :preview-key "C-o"
   consult-grep :preview-key "C-o"
   ;; For `consult-line' change the prompt and specify multiple preview
   ;; keybindings. Note that you should bind <S-up> and <S-down> in the
   ;; `minibuffer-local-completion-map' or `vertico-map' to the commands which
   ;; select the previous or next candidate.
   consult-ripgrep :preview-key "C-o"
   consult-line :preview-key '(:debounce 0.0 any)
   )
  )

(use-package! origami
  :config
  (add-hook 'yaml-mode-hook 'origami-mode)
  )

(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package! savehist
  :init
  (savehist-mode))

(after! swiper
  :config
  (global-set-key "\C-s" 'swiper-isearch)
  )

(after! ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-x f") '+ivy/projectile-find-file)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-S-SPC") 'ivy-switch-buffer)
  (global-set-key (kbd "C-M-s") 'counsel-git-grep)
  )

(after! all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  )

(after! helm
  :config
  (map!
   :leader
   "C-S-SPC" #'helm-buffers-list
   "C-x C-f" #'helm-find-files
   ;; "M-x" #'helm-M-x
   "M-RET" #'helm-imenu
   )

  (setq helm-mode t)
  (setq helm-split-window-in-side-p t)
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _)
    candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)

  (setq helm-boring-buffer-regexp-list (list
                                        (rx "*magit-") (rx "*helm") (rx "*py") (rx "*echo")
                                        (rx "*Fly") (rx "*mini") (rx "*Qua") (rx "*Neo")
                                        (rx "*Compa") (rx "*code") (rx "*http")
                                        (rx "*anaco") (rx "*tip") (rx "*xwi") (rx "magit-")
                                        (rx "*server") (rx "*which")))
  )

;; yay -S tree-sitter
(use-package! tree-sitter
  :config
  (tree-sitter-require 'python)
  (tree-sitter-require 'javascript)
  (tree-sitter-require 'html)
  (tree-sitter-require 'rust)
  ;; (global-tree-sitter-mode)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode)
  )

(use-package! tree-sitter-langs)

(use-package! yasnippet
  :config
  (setq yas-snippet-dirs (list
                          (concat settings_path "snippets")
                          ))
  (yas-global-mode 1)
  )

;; automatic and manual symbol highlighting for Emacs
(use-package! highlight-symbol
  :config
  (setq highlight-symbol-idle-delay 0)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'text-mode-hook 'highlight-symbol-mode)
  (add-hook 'python-mode-hook 'highlight-symbol-mode)
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q") 'highlight-symbol-next)
  (global-set-key (kbd "C-S-q") 'highlight-symbol-prev)
  )

(use-package! magit
  :config
  (global-set-key (kbd "C-c RET") 'magit-status)
  (global-set-key (kbd "C-c m") 'magit-status)
  (global-set-key (kbd "C-x v h") 'magit-log-buffer-file)
  (global-set-key (kbd "C-x v b") 'magit-blame)

  (define-key magit-mode-map (kbd "<C-tab>") (lambda () (interactive) (other-window 1)))
  (define-key magit-mode-map (kbd "M-1") 'winum-select-window-1)
  (define-key magit-mode-map (kbd "M-2") 'winum-select-window-2)
  (define-key magit-mode-map (kbd "M-3") 'winum-select-window-3)
  (define-key magit-mode-map (kbd "M-4") 'winum-select-window-4)
  (define-key magit-mode-map (kbd "1") 'magit-section-show-level-1-all)
  (define-key magit-mode-map (kbd "2") 'magit-section-show-level-2-all)
  (define-key magit-mode-map (kbd "3") 'magit-section-show-level-3-all)
  (define-key magit-mode-map (kbd "4") 'magit-section-show-level-4-all)

  (define-key smerge-mode-map (kbd "<down>") 'smerge-next)
  (define-key smerge-mode-map (kbd "<up>") 'smerge-prev)
  (define-key smerge-mode-map (kbd "с") 'smerge-keep-current)
  (define-key smerge-mode-map (kbd "m") 'smerge-keep-mine)
  (define-key smerge-mode-map (kbd "o") 'smerge-keep-other)
  (define-key smerge-mode-map (kbd "r") 'smerge-resolve)
  (define-key smerge-mode-map (kbd "R") 'smerge-resolve-all)

  (setq magit-display-buffer-function #'magit-display-buffer-traditional)
  (setq projectile-switch-project-action 'magit-status)
  )


(use-package! winum
  :config
  ;; (setq winum-ignored-buffers-regexp (list (rx "*neotree*")))
  (setq window-numbering-scope 'global)
  (winum-mode)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  )

(use-package! multiple-cursors
  :config
  ;; Multicursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-с C->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-с C-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "<C-C-down-mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-c <mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-c n") 'mc/insert-numbers)
  ;; (global-set-key (kbd "C-c l") 'mc/insert-letters)
  ;; (global-set-key (kbd "C-c c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this-word) ; choose same word next
  (global-set-key (kbd "C-c C-p") 'mc/mark-previous-word-like-this) ; choose same word previous
  )

;; 1 - lsp-mode
;; 2 - lsp-bridge
(setq lsp-type 1)

;; sudo pip install 'python-language-server[all]'
(if (eq lsp-type 1)
  (progn
    (use-package! lsp-mode
      :custom
      (lsp-headerline-breadcrumb-enable nil)
      :config
      (setq lsp-auto-guess-root t)
      (add-hook 'rust-mode-hook #'lsp)
      (add-hook 'python-mode-hook #'lsp)

      (setq lsp-pyls-plugins-jedi-references-enabled t)
      ;; (setq lsp-pyls-server-command (quote ("pyls")))

      (setq lsp-document-highlight-delay 0.1)
      (setq lsp-enable-semantic-highlighting t)
      (setq lsp-enable-symbol-highlighting t)
      (setq lsp-symbol-highlighting-skip-current t)

      (setq lsp-enable-indentation nil)
      (setq format-with-lsp nil)
      (setq lsp-enable-snippet t)
      (setq lsp-prefer-flymake nil)
      (lsp-enable-which-key-integration t)

      (setq lsp-signature-auto-activate nil)

      (setq lsp-diagnostics-provider nil)

      (setq lsp-diagnostics-provider :none)

      ;; (define-key lsp-mode-map (kbd "C-i") 'lsp-describe-thing-at-point)
      (define-key lsp-mode-map (kbd "C-o") 'lsp-find-definition)
      (define-key lsp-mode-map (kbd "C-S-SPC") 'consult-buffer)
      (define-key lsp-mode-map (kbd "C-SPC") 'lsp-execute-code-action)

      (setq lsp-modeline-diagnostics-scope :project)

      (setq lsp-enable-file-watchers nil)
      )

    (use-package! lsp-ui
      :config
      (add-hook 'lsp-mode-hook #'lsp-ui-mode)
      (add-hook 'lsp-mode-hook #'lsp-ui-doc-mode)

      (setq lsp-ui-doc-enable nil)
      (setq lsp-ui-flycheck-enable nil)
      (setq lsp-ui-peek-enable nil)
      (setq lsp-ui-sideline-enable nil)
      (setq lsp-ui-doc-alignment (quote frame))
      (setq lsp-ui-doc-delay 0.2)
      (setq lsp-ui-doc-max-height 30)
      (setq lsp-ui-doc-max-width 100)
      (setq lsp-ui-doc-use-webkit t)
      (setq lsp-ui-doc-position 'at-point)
      (define-key lsp-mode-map (kbd "C-o") 'lsp-ui-peek-find-definitions)
      (define-key lsp-mode-map (kbd "C-i") 'lsp-ui-doc-show)
      (define-key lsp-mode-map (kbd "C-r r") 'lsp-ui-peek-find-references)
      (define-key lsp-mode-map (kbd "C-r i") 'lsp-ui-peek-find-implementation)
      (define-key lsp-mode-map (kbd "<C-M-return>") 'lsp-ui-imenu)
      )
    )
  )

(if (eq lsp-type 2)
  (progn
    (use-package! posframe)
    (use-package! markdown-mode)
    (use-package! popon)

    (use-package! lsp-bridge
      :init
      (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge")
      (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge/acm")
      :config
      (global-lsp-bridge-mode)

      (setq lsp-bridge-python-lsp-server :pylsp)
      (setq acm-backend-lsp-enable-auto-import t)
      (setq acm-markdown-render-font-height 80)

      (define-key lsp-bridge-mode-map (kbd "C-o") 'lsp-bridge-find-def)
      (define-key lsp-bridge-mode-map (kbd "C-i") 'lsp-bridge-popup-documentation)
      (define-key lsp-bridge-mode-map (kbd "C-S-SPC") 'consult-buffer)
      (define-key lsp-bridge-mode-map (kbd "M-h") 'backward-delete-word)
      (define-key lsp-bridge-mode-map (kbd "C-SPC") 'lsp-bridge-code-action)
      )
    )
  )

(defvar python-mode-map)

;; https://github.com/timothycrosley/isort
;; ~/.isort.cfg
;; [settings]
;; multi_line_output=4
(after! python
  (use-package py-isort
    :config
    (define-key python-mode-map (kbd "C-c C-o") 'py-isort-buffer)
    )
  )

;; yay -S python python-pip pyright python-lsp-server ruff python-virtualenv python-psycopg2 python-black
;; pip install epc orjson sexpdata six orjson pyright python-lsp-server[all] rope ruff ruff-lsp flake8 mypy pylint isort virtualenvwrapper virtualenv==20.0.23 "python-lsp-server[all]" setuptools pipenv --break-system-packages
;; python -m ensurepip --default-pip
;;
;; Add this to your .bashrc / .bash_profile / .zshrc:
;; # load virtualenvwrapper for python (after custom PATHs)
;; source ~/.local/bin/virtualenvwrapper.sh

;; VIRTUALENVWRAPPER_PYTHON="$(command \which python)"3

;; mkvirtualenv --python=python default
(after! python
  (use-package! virtualenvwrapper
    :config
    (venv-projectile-auto-workon)

    (venv-initialize-interactive-shells) ;; if you want interactive shell support
    (venv-initialize-eshell) ;; if you want eshell support
    ;; note that setting `venv-location` is not necessary if you
    ;; use the default location (`~/.virtualenvs`), or if the
    ;; the environment variable `WORKON_HOME` points to the right place
    (setq venv-location "~/.virtualenvs/")
    (define-key python-mode-map (kbd "C-c a") 'venv-workon)
    )
  )

;; Install rust
;; curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; yay -S rust-analyzer
(use-package! rust-mode
  :config
  (define-key rust-mode-map (kbd "C-c C-o") 'rust-format-buffer)
  )

(use-package! rust-cargo)

;; PATH=$PATH:~/.cargo/bin/cargo
(use-package! flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

(use-package! neotree
  :config
  (setq neo-autorefresh nil)

  (setq neo-theme (if (display-graphic-p) 'icons))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  ;; (global-set-key (kbd "<f8>") 'neotree-project-dir)
  (global-unset-key (kbd "C-t"))
  ;; (global-set-key (kbd "C-t") 'neotree-toggle)
  (global-set-key (kbd "C-t") 'neotree-project-dir)
  (setq neo-window-fixed-size t)
  (setq neo-window-width 30)
  )

(after! flycheck
  ;; flycheck-verify-setup
  :config
  (global-flycheck-mode)  ;; (setq flycheck-highlighting-mode (quote symbols))

  (global-set-key (kbd "M-z") 'flycheck-previous-error)
  (global-set-key (kbd "C-z") 'flycheck-next-error)
  (global-set-key (kbd "C-M-z") 'flycheck-copy-errors-as-kill)  ;; (flycheck-add-next-checker 'python-pylint 'python-mypy)

  (setq flycheck-python-mypy-config `("mypy.ini" ,(concat settings_path "configs/mypy.ini")))
  (setq flycheck-flake8rc (concat settings_path "configs/flake8rc"))
  (setq flycheck-pylintrc (concat settings_path "configs/.pylintrc"))

  (setq-default flycheck-disabled-checkers '(python-mypy, lsp, python-pylint, python-pycompile))
  (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  )

(after! company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-quickhelp-delay 0)
  (setq company-show-numbers 1)
  (setq company-show-location 1)
  (company-quickhelp-mode 1)

  (define-key lsp-mode-map (kbd "<tab>") 'company-complete)
  (push 'company-elisp company-backends)

  (use-package! company-quickhelp
    :config
    (company-quickhelp-mode)
    )
  )

(use-package! centaur-tabs
  :hook
  (neotree-mode . centaur-tabs-local-mode)
  (Lsp-Ui-Doc-Frame . centaur-tabs-local-mode)
  (Async-Bytecomp-Package . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "rounded")
  (setq centaur-tabs-height 20)
  (setq centaur-tabs-set-icons nil)
  (setq centaur-tabs-gray-out-icons 'buffer)

  (setq centaur-tabs-adjust-buffer-order nil)

  (global-set-key (kbd "<C-M-tab>") 'centaur-tabs-forward)
  (global-set-key (kbd "<C-M-iso-lefttab>") 'centaur-tabs-backward)

  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((memq major-mode '(
                          magit-process-mode magit-status-mode
                          magit-diff-mode magit-log-mode
                          magit-file-mode magit-blob-mode
                          magit-blame-mode))
       "Emacs")
      ((derived-mode-p 'emacs-lisp-mode) "Lisp")
      ((derived-mode-p 'shell-mode) "Shell")
      ((derived-mode-p 'eshell-mode) "Shell")
      ((derived-mode-p 'aweshell-mode) "Shell")
      ((derived-mode-p 'vterm-mode) "Shell")
      ((derived-mode-p 'python-mode) "Python")
      ((derived-mode-p 'web-mode) "Web")
      ((memq major-mode '(
                          org-mode org-agenda-clockreport-mode
                          org-src-mode org-agenda-mode
                          org-beamer-mode org-indent-mode
                          org-bullets-mode org-cdlatex-mode
                          org-agenda-log-mode diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
     ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*vc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*ruff-lsp*" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*pyright" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*dashboard*" name)

       (cl-search "*company" name)
       (cl-search "*lsp" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  (centaur-tabs-group-buffer-groups)

  (global-set-key (kbd "C-c f") 'centaur-tabs--copy-file-name-to-clipboard)

  (centaur-tabs-group-by-projectile-project)
  )

(use-package! undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode)
  )

(use-package! restclient)

(use-package emmet-mode
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)

  (setq emmet-preview-default nil)
  )

(use-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'bitmap)

  (highlight-indent-guides-mode nil)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode)
  )

(after! js2-mode
  :config
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))
  )

(use-package! web-mode
  :config

  (define-key web-mode-map (kbd "C-o") 'emmet-expand-line)
  (define-key web-mode-map (kbd "C-c RET") nil)

  (setq web-mode-enable-css-colorization nil)
  (setq web-mode-enable-auto-indentation nil)

  (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight nil)
  (setq web-mode-enable-whitespace-fontification nil)
  (setq web-mode-enable-element-tag-fontification nil)
  (setq web-mode-enable-block-face nil)
  (setq web-mode-enable-part-face nil)

  (setq web-mode-part-padding 0)
  (setq web-mode-script-padding 0)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
  )

(use-package! page-break-lines
  :config
  (page-break-lines-mode)
  )

(use-package! dashboard
  :init
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-items '((bookmarks . 15)
                          (recents . 10)
                          ;(agenda . 5)
                          (projects . 3)
                          ;(registers . 3)
                          ))
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  )
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package! expand-region
  :config
  (global-set-key (kbd "C-j") 'er/expand-region)
  (global-set-key (kbd "C-S-J") (lambda () (interactive) (er/expand-region -1)))
  )

(use-package! reverse-im
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t)
  )

(use-package! smartparens
  :config
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'web-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'css-mode-hook #'smartparens-mode)
  (add-hook 'yaml-mode-hook #'smartparens-mode)
  (add-hook 'js2-mode #'smartparens-mode)
  )

(use-package! org
  :config
  ;; (setq org-log-done 'time)

  (setq org-support-shift-select t)

  (define-key org-mode-map (kbd "C-c l") 'org-scontexttore-link)
  (define-key org-mode-map (kbd "C-h") 'delete-backward-char)
  (define-key org-mode-map (kbd "M-h") 'backward-delete-word)

  (define-key org-mode-map (kbd "C-c a") 'org-clock-in)
  (define-key org-mode-map (kbd "C-c e") 'org-clock-out)
  (define-key org-mode-map (kbd "C-c c") 'org-clock-in-last)

  (define-key org-mode-map (kbd "C-i") 'org-shiftright)
  (define-key org-mode-map (kbd "C-S-i") 'org-shiftleft)

  (define-key org-mode-map (kbd "C-o") 'org-metaright)
  (define-key org-mode-map (kbd "C-S-o") 'org-metaleft)

  (define-key org-mode-map (kbd "<C-tab>") (lambda () (interactive) (other-window 1)))
  (define-key org-mode-map (kbd "<C-iso-lefttab>") (lambda () (interactive) (other-window -1)))

  (setq org-todo-keywords
        '((sequence "TODO" "IN" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "dodger blue" :weight bold))
          ("IN" . (:foreground "lawn green" :weight bold))
          ("DONE" . (:foreground "dim gray" :weight bold))
          ))

  (setq org-agenda-files (list (concat dropbox_path "org_files")))
  (global-set-key (kbd "C-x n !") (lambda() (interactive)(find-file (concat dropbox_path "org_files/main.org"))))
  (global-set-key (kbd "C-x n @") (lambda() (interactive)(find-file (concat dropbox_path "org_files/work.org"))))
  (custom-set-faces '(org-link ((t (:underline "dodger blue" :foreground "dodger blue")))))
  (add-hook 'org-mode-hook #'(lambda ()
                               (visual-line-mode)
                               (org-indent-mode)))
  )

(use-package! undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t)
  )

;; yay -S hunspell
(use-package! flyspell-correct
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
  )

;; pip install black
;; python-black-buffer
(use-package! python-black
  :config
  (setq python-black-extra-args `(
                                 "--skip-string-normalization"
                                 ,(concat "--config=" settings_path "configs/black.toml")
                                 ))
  )

(use-package! atomic-chrome
  :config
  (atomic-chrome-start-server)
  )

(use-package! linum
  :config
  ;; Alternatively, to use it only in programming modes:
  (add-hook 'python-mode-hook #'linum-mode)
  (add-hook 'rust-mode-hook #'linum-mode)
  )

(load! (concat settings_path "settings/functions.el"))

;; Use ripgrep in Emacs.
(use-package! rg)
