;;; Settings --- symmary:
;;; Commentary:
;;; Code:
(message "Init main.py")

;; This tells Emacs not to warn you about anything except problems
(setq warning-minimum-level :emergency)

;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)
(setq desktop-path (list "~/.emacs.d/"))
(desktop-save-mode 1)

(setq package-check-signature nil)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
(set-frame-parameter nil 'undecorated nil)

;; How to get rid of "Loading a theme can run Lisp code. Really load? (y or n) " message?
(set-variable 'sml/no-confirm-load-theme t)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; How to overwrite text by yank in Emacs?
(delete-selection-mode 1)

(menu-bar-mode -1)

;; To make the cursor even more visible
(global-hl-line-mode)

(setq ring-bell-function 'ignore)

(display-time-mode 1)

;; Finally you can toggle the display of scroll bars on all frames
(scroll-bar-mode -1)

;; if you want it for every buffer
(global-linum-mode t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; (global-visual-line-mode -1)

;; From Pragmatic Emacs a more concise way to kill the buffer.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(when (string-equal system-type "windows-nt")
  (setenv "HOME" (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH"))))

;; If you enable winner-mode, you get something akin to a stack-based
;; undo/redo functionality for all your window configuration changes.
;; By default, C-c <left> gets bound to winner-undo, while C-c <right> performs winner-redo.
(winner-mode 1)
(global-set-key (kbd "C-c [") `wg-undo-wconfig-change)
(global-set-key (kbd "C-c ]") `wg-redo-wconfig-change)

(tool-bar-mode -1)

;; Note that this will affect all histories, not just the shell.
;; Save sessions history
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring)
      savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
(setq history-delete-duplicates t)
(setq comint-input-ignoredups t)

;; (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
;; (define-key shell-mode-map (kbd "<down>") 'comint-next-input)

(set-variable 'dov-view-continues t)

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

(show-paren-mode 1)

(add-to-list 'completion-styles 'initials t)

(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 85 :width normal)))))

(message "Init base hotkeys")

;; (global-set-key (kbd "C-x s") 'rgrep)

;; (global-set-key (kbd "<C-M-tab>") 'next-buffer)
;; (global-set-key (kbd "<C-M-iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "<C-M-tab>") 'tabbar-forward)
(global-set-key (kbd "<C-M-iso-lefttab>") 'tabbar-backward)

(global-set-key (kbd "<C-tab>") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "<C-iso-lefttab>") (lambda () (interactive) (other-window -1)))

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

(global-set-key (kbd "C-x C-d") (lambda() (interactive)(find-file (concat settings_path "main.el"))))

(global-set-key (kbd "C-c h") `whitespace-mode)

(global-set-key (kbd "C-x c") `list-colors-display)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "C-c r") `revert-buffer)
;; (global-set-key (kbd "C-r") `replace-string)

;; (global-set-key (kbd "C-x r r") `bookmark-jump)
(global-set-key (kbd "C-x r s") `bookmark-set)
(global-set-key (kbd "C-x r d") `bookmark-delete)

(global-set-key (kbd "C-c b") 'ibuffer-list-buffers)

(global-unset-key (kbd "C-o"))

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-.") `undo)
(global-set-key (kbd "C-x t") 'dired)
;; (global-set-key (kbd "<C-right>") 'py-shift-right)
;; (global-set-key (kbd "<C-left>") 'py-shift-left)

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
(global-set-key (kbd "C-M-p")
                (lambda () (interactive) (previous-with-center 5)))

;; Windows/frames
(global-set-key (kbd "<C-S-right>") ' enlarge-window-horizontally)
(global-set-key (kbd "<C-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'shrink-window)

(add-hook 'before-save-hook
  (lambda ()
    (untabify (point-min) (point-max))
    ))

(message "Init use-package")
(require 'use-package)

(use-package realgud
  :ensure t
  )

(use-package bui
  :ensure t
  )

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (list
                          (concat settings_path "snippets")
                          ))
  (yas-global-mode 1)
  )

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  )

;; w3m
(use-package w3m
  :ensure t
  :config
  (setq w3m-user-agent "Mozilla/5.0 (Linux; U; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
  (global-set-key (kbd "C-x w") (lambda() (interactive)(w3m-browse-url "https://www.google.com/webhp?nomo=1&hl=ru")))
  )

;; w3m
(pretty-hydra-define hydra-w3m
  (:color blue)
  ("Navigation"
   (("B" w3m-view-previous-page)
    ("N" w3m-view-next-page)
    ("[" w3m-previous-form)
    ("]" w3m-next-form)
    ("M" w3m-view-url-with-browse-url)
    ("R" w3m-reload-this-page)
    ("\\" w3m-view-source)
    )
   "Bookmarks"
   (("a" w3m-bookmark-add-current-url)
    ("M-a" w3m-bookmark-add-this-url)
    ("v" w3m-bookmark-view)
    ))
  )

(define-key w3m-mode-map (kbd "C-x b") #'hydra-w3m/body)

(use-package smerge-mode
  :ensure t
  )

;; smerge
(setq smerge-command-prefix "\C-cv")
(pretty-hydra-define hydra-smerge
  (:color blue)
  ("smerge-command-prefix +"
   (("n" smerge-next)
    ("p" smerge-previous)
    ("RET" smerge-keep-current)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("E" smerge-ediff)
    )
   "Bookmarks"
   (("a" w3m-bookmark-add-current-url)
    ("M-a" w3m-bookmark-add-this-url)
    ("v" w3m-bookmark-view)
    ))
  )

(define-key smerge-mode-map (kbd "C-x b") #'hydra-smerge/body)

(use-package hydra
  :ensure t
  )

(use-package pretty-hydra
  :ensure t
  )

(use-package eww
  :ensure t
  )
;; (global-set-key (kbd "C-x w") (lambda() (interactive)(eww "google.com")))
(define-key eww-mode-map (kbd "e") #'eww-browse-with-external-browser)

(pretty-hydra-define hydra-eww
  (:color blue)
  ("Navigation"
   (("p" eww-previous-url)
    ("r" eww-forward-url)
    ("e" eww-browse-with-external-browser)
    ("g" eww-reload)
    ("w" eww-copy-page-url)
    ("v" eww-view-source)
    ))
  )

(define-key eww-mode-map (kbd "C-x b") #'hydra-eww/body)

(use-package evil
  :ensure t
  )

;; (use-package perspective
;;   :ensure t
;;   :config
;;   (persp-mode)
;;   )

(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice)
  )

(use-package window-number
  :ensure t
  :quelpa (window-number :fetcher github :repo "nikolas/window-number")
  )

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-x o") 'dumb-jump-go)
;;   )

(use-package smart-jump
  :ensure t
  :config
  ;; (global-set-key (kbd "C-x o") 'smart-jump-go)
  )

(use-package tabbar
  :ensure t
  :config (tabbar-mode)
  )

(use-package yasnippet
  ;; template system for Emacs
  :ensure t
  :config
  )

(use-package tabbar-ruler
  :ensure t
  :quelpa (tabbar-ruler :fetcher github :repo "mattfidler/tabbar-ruler.el")
  :config
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
  (setq tabbar-ruler-global-ruler nil) ; if you want a global ruler
  ;; (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
  ;; (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
  ;; (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
  (tabbar-ruler-group-by-projectile-project)
  (tabbar-ruler-group-buffer-groups)

  (setq tabbar-ruler-excluded-buffers
    (quote
     ("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*" "*helm-mini*" "*helm-mode-describe-variable*" "*anaconda-mode*" "*Anaconda*" "*Compile-Log*" "*grep*" "*pyls*" "*pyls::stderr*" "*rls*" "*rls::stderr*" "*eglot*" "*EGLOT*" "magit*")))
  )

(window-number-meta-mode)
(window-number-mode 1)

;; Available only on mercurial versions 1.9 or higher
(setq monky-process-type 'cmdserver)
(global-set-key (kbd "C-c l") 'monky-status)

;; centered-cursor-mode
(global-set-key (kbd "<Scroll_Lock>") 'centered-cursor-mode)
(global-set-key (kbd "<C-Scroll_Lock>") 'scroll-lock-mode)


(add-to-list 'custom-theme-load-path (concat settings_path "emacsd/themes/"))

;; Hideshow
(global-set-key (kbd "C-c C-g") 'hs-show-all)
(global-set-key (kbd "C-c C-h") 'hs-hide-all)
(add-hook 'ropemacs-mode-hook     'hs-minor-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; Install the Git frontend Magit
;; git config --global status.showUntrackedFiles all
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'magit-status)
  (global-set-key (kbd "C-c C-m") 'magit-dispatch-popup)
  (global-set-key (kbd "C-x v h") 'magit-log-buffer-file)
  (global-set-key (kbd "C-x v b") 'magit-blame)
  )

;; (use-package request
;;   :ensure t)

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

(use-package multiple-cursors
  :ensure t
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
  (global-set-key (kbd "C-c c") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this-word) ; choose same word next
  (global-set-key (kbd "C-c C-p") 'mc/mark-previous-word-like-this) ; choose same word previous
  )

(use-package back-button
  :ensure t
  :config
  (back-button-mode 1)
  ;; Back navigation
  (global-set-key (kbd "M-n") 'back-button-local-forward)
  (global-set-key (kbd "M-p") 'back-button-local-backward)
  ;; (global-set-key (kbd "<M-right>") 'back-button-global-forward)
  ;; (global-set-key (kbd "<M-left>") 'back-button-global-backward)
  (global-set-key (kbd "C-M-[") 'back-button-global-forward)
  (global-set-key (kbd "C-M-]") 'back-button-global-backward))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq-default fill-column 80)
;;   ;; (global-set-key (kbd "C-c s") 'fci-mode)
;;   (add-hook 'ropemacs-mode-hook 'fci-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-h") 'backward-delete-char-untabify)
  )

(use-package lsp-mode
  :quelpa (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
  :ensure t
  :config
  (setq lsp-auto-guess-root t)
  ;; (add-hook 'rust-mode-hook #'lsp)

  ;; sudo pip install 'python-language-server[all]'
  (add-hook 'python-mode-hook #'lsp)
  (setq lsp-pyls-plugins-jedi-references-enabled nil)
  (setq lsp-pyls-server-command (quote ("pyls")))

  (setq lsp-document-highlight-delay 0.1)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-symbol-highlighting-skip-current t)

  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet t)
  (setq lsp-prefer-flymake nil)
  
  (define-key python-mode-map (kbd "C-i") 'lsp-describe-thing-at-point)
  ;; (define-key python-mode-map (kbd "C-o") #'lsp-find-definition)
    
  (define-key python-mode-map (kbd "C-r r") 'lsp-ui-peek-find-references)
  (define-key python-mode-map (kbd "C-r i") 'lsp-ui-peek-find-implementation)
  )

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode nil)
  )

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends)
  )

(use-package lsp-ui
  :ensure t
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  
  (setq lsp-ui-doc-alignment (quote frame))
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-use-webkit nil)
  
  (global-set-key (kbd "<C-M-return>") 'lsp-ui-imenu)
  
  (define-key python-mode-map (kbd "C-o") #'lsp-ui-peek-find-definitions)
  (define-key rust-mode-map (kbd "C-o") #'lsp-ui-peek-find-definitions)
  )

;; (use-package dap-mode
;;   :ensure t
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   ;; enables mouse hover support
;;   (dap-tooltip-mode 1)
;;   ;; use tooltips for mouse hover
;;   ;; if it is not enabled `dap-mode' will use the minibuffer.
;;   (tooltip-mode 1)
;; 
;;   (require 'dap-lldb)
;; 
;;   ;; pip install "ptvsd>=4.2"
;;   (require 'dap-python)
;;   )

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list
;;   )

;; Rust
(message "Rust")
;; rustup component add rust-src
;; cargo +nightly install racer
;; rustup toolchain add nightly
;; rustup component add rls rust-analysis rust-src
(use-package rust-mode
  :ensure t
  :config
  ;; cargo install racer
  (setq racer-cmd "~/.cargo/bin/racer")

  ;; rustup component add rust-src
  ;; rustc --print sysroot /lib/rustlib/src/rust/src
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

  ;; Configure Emacs to activate racer when rust-mode starts
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  
  ;; (define-key rust-mode-map (kbd "C-i") #'racer-describe-tooltip)
  (define-key rust-mode-map (kbd "C-i") #'lsp-describe-thing-at-point)
  
  ;; (define-key rust-mode-map (kbd "C-o") #'racer-find-definition)

  (define-key rust-mode-map (kbd "C-r r") 'lsp-ui-peek-find-references)
  (define-key rust-mode-map (kbd "C-r i") 'lsp-ui-peek-find-implementation)
  )

(use-package racer
  :ensure t
  )

(use-package ron-mode
  :quelpa (ron-mode :fetcher github :repo "rhololkeolke/ron-mode")
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ron\\'" . ron-mode))
  )

(use-package cargo
  :ensure t
  )

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Smartparens is a minor mode for dealing with pairs in Emacs. 
;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (smartparens-global-mode))

;; (use-package bm
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "<C-f2>") 'bm-toggle)
;;   (global-set-key (kbd "<f2>")   'bm-next)
;;   (global-set-key (kbd "<S-f2>") 'bm-previous))

(use-package dockerfile-mode
  :quelpa (dockerfile-mode :fetcher github :repo "spotify/dockerfile-mode")
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist '(("en" . "ru")))
  ;; (global-set-key "\C-ct" 'google-translate-smooth-translate)
  )

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(use-package dired-sidebar
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'dired-sidebar-show-sidebar)
  )

(use-package neotree
  :ensure t
  :config
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
  )

;; https://github.com/emacs-pe/company-racer
(use-package company-racer
  :ensure t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer))
  )

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  )

;; (use-package ess
;;   :ensure t)

(use-package helm
  :ensure t
  :init
  (setq helm-mode t)
  :config
  (setq helm-split-window-in-side-p t)
  ;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-SPC") 'helm-buffers-list)
  (global-unset-key (kbd "C-x C-f"))
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "M-RET") 'helm-imenu)
  (define-key helm-map (kbd "C-h") nil)
  (global-set-key (kbd "C-x r r") #'helm-filtered-bookmarks)

  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _)
    candidates)

  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)
  (setq helm-boring-buffer-regexp-list (list
                                        (rx "*magit-") (rx "*helm") (rx "*py") (rx "*echo")
                                        (rx "*Fly") (rx "*mini") (rx "*Qua") (rx "*Neo")
                                        (rx "*Compa") (rx "*code") (rx "*http")))
  )

(use-package restclient
  :ensure t
  :config
  (define-key restclient-mode-map (kbd "M-RET") 'helm-restclient)
  )

(use-package helm-projectile
  :ensure t
  :config
  ; (global-set-key (kbd "C-x s") 'helm-projectile-grep)
  (global-unset-key (kbd "C-M-j"))
  (global-set-key (kbd "C-M-j") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-S-SPC") 'helm-projectile-switch-to-buffer)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :ensure t
  :config
  ;; (counsel-projectile-mode)
  (global-set-key (kbd "C-x p f") 'counsel-projectile-find-file)
  (global-set-key (kbd "C-M-s") 'counsel-projectile-git-grep)
  (defun counsel-projectile-git-grep-py ()
    (interactive)
    (setq current-prefix-arg '"git --no-pager grep --full-name -n --no-color -i -I -e \"%s\" -- \"*.py\"")
    (call-interactively 'counsel-projectile-git-grep))
  (global-set-key (kbd "C-x s") 'counsel-projectile-git-grep-py)
  (global-set-key (kbd "C-x f") 'counsel-projectile-find-file)
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  )

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-j") 'er/expand-region)
  (global-set-key (kbd "C-S-J") (lambda () (interactive) (er/expand-region -1)))
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-julia-setup)
  (setq flycheck-disabled-checkers nil)
  (setq flycheck-highlighting-mode (quote symbols))

  (global-set-key (kbd "M-z") 'flycheck-previous-error)
  (global-set-key (kbd "C-z") 'flycheck-next-error)
  (global-set-key (kbd "C-M-z") 'flycheck-copy-errors-as-kill)
  )

(use-package flycheck-julia
  :ensure t
  )

(use-package flycheck-mypy
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))

(message "Web-mode")
(use-package web-mode
  :ensure t)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (define-key web-mode-map (kbd "C-o") 'emmet-expand-line)
  )

(use-package epc
  :ensure t)

;; sudo apt-get install sqlformat
(use-package format-sql
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package csv-mode
  :ensure t)

(message "Python")

;; pip install isort
;; https://github.com/timothycrosley/isort
;; ~/.isort.cfg multi_line_output=4 - Hanging Grid
(use-package py-isort
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'py-isort-buffer)
  )

(setq python-python-command "/home/gagen/.virtualenvs/gc3/bin/python3.6")

(define-key python-mode-map (kbd "<tab>") 'python-indent-shift-right)
(define-key python-mode-map (kbd "<backtab>") 'python-indent-shift-left)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  ;; note that setting `venv-location` is not necessary if you
  ;; use the default location (`~/.virtualenvs`), or if the
  ;; the environment variable `WORKON_HOME` points to the right place
  (setq venv-location "~/.virtualenvs/")
  (global-set-key (kbd "C-c a") 'venv-workon)
  (setq venv-location '("~/.virtualenvs/py3/"
                        "~/.virtualenvs/gc/"))
  )

;; (use-package eglot
;;   :quelpa (eglot :fetcher github :repo "joaotavora/eglot")
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'eglot-ensure)
;;   (add-to-list 'eglot-server-programs
;;              `(python-mode . ("pyls" "-v" "--tcp" "--host"
;;                               "localhost" "--port" :autoport)))
;;   )

;; (use-package auto-virtualenv
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
;;   (add-hook 'pyvenv-post-activate-hooks 'wcx-restart-python)
;;   )

;; pip install virtualenvwrapper
;; source /usr/local/bin/virtualenvwrapper.sh
;; Add this to your .bashrc / .bash_profile / .zshrc:
;; # load virtualenvwrapper for python (after custom PATHs)
;; source /home/user/.local/bin/virtualenvwrapper.sh

;; mkvirtualenv env1
;; mkvirtualenv --python=python3.7 test3
;; pip install 'python-language-server[all]'

;; pylint --generate-rcfile > ~/.pylintrc
;; E0401,C0111,R0903,W0613,C0103

(add-hook 'python-mode-hook
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  )

(pretty-hydra-define hydra-python
  (:color blue)
  ("IDE"
   (("C-x o" pyimport-insert-missing)
   ("C-x i" pyimport-remove-unused)
    ))
  )

(define-key python-mode-map (kbd "C-x b") #'hydra-python/body)

;; ITS BAD (if you as me)
;; (use-package python-mode
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
;;   )

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (defun my/ropemacs-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;; 
;;   (add-hook 'ropemacs-mode-hook 'my/ropemacs-mode-hook)
;;   
;;   ;; (add-hook 'ropemacs-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   
;;   (define-key jedi-mode-map (kbd "C-o") 'jedi:goto-definition)
;;   ;; (define-key jedi-mode-map (kbd "<tab>") 'jedi:complete)
;;   (define-key jedi-mode-map (kbd "C-i") 'jedi:show-doc-in-tip)
;; 
;;   (add-hook 'jedi-mode-hook 'jedi-direx:setup)
;;   (add-hook 'ropemacs-mode-hook 'auto-complete-mode)
;;   (add-hook 'ropemacs-mode-hook 'jedi:ac-setup)
;;   (setq elpy-rpc-backend "jedi")
;;   (eval-after-load 'ropemacs-mode-hook
;;     '(define-key jedi-mode-map (kbd "TAB") 'jedi:complete))
;;   
;;   (setq jedi:server-args
;;       '("--virtual-env" "~/.virtualenvs/gc"
;;      ))
;;   )

;; (use-package anaconda-mode
;;   :ensure t
;;   :config
;;   (define-key python-mode-map (kbd "C-o") 'anaconda-mode-find-definitions)
;;   
;;   (define-key python-mode-map (kbd "C-i") 'anaconda-mode-show-doc)
;;   
;;   ;; (define-key python-mode-map (kbd "C-c a") 'pythonic-activate)
;; 
;;   (add-to-list 'python-shell-extra-pythonpaths "~/.virtualenvs/gc")
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   )

;; Requires pyflakes to be installed.
;; This requires pyflakes to be on PATH. Alternatively, set pyimport-pyflakes-path.
(use-package pyimport
  :ensure t
  :config
  (define-key python-mode-map (kbd "C-x o") 'pyimport-insert-missing)
  (define-key python-mode-map (kbd "C-x i") 'pyimport-remove-unused)
  )

(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  )

(use-package jedi-direx
  :ensure t
  )

;; (use-package omnisharp
;;   :ensure t
;;   :config
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (eval-after-load
;;     'company
;;     '(add-to-list 'company-backends #'company-omnisharp))
;; 
;;   (defun my-csharp-mode-setup ()
;;     (omnisharp-mode)
;;     (company-mode)
;;     (flycheck-mode)
;; 
;;     (setq indent-tabs-mode nil)
;;     (setq c-syntactic-indentation t)
;;     (c-set-style "ellemtel")
;;     (setq c-basic-offset 4)
;;     (setq truncate-lines t)
;;     (setq tab-width 4)
;;     (setq evil-shift-width 4)
;; 
;;     ;csharp-mode README.md recommends this too
;;     ;(electric-pair-mode 1)       ;; Emacs 24
;;     ;(electric-pair-local-mode 1) ;; Emacs 25
;; 
;;     (define-key omnisharp-mode-map (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
;;     (define-key omnisharp-mode-map (kbd "C-c s") 'omnisharp-start-omnisharp-server)
;;     (define-key omnisharp-mode-map (kbd "C-c C-c") 'recompile))
;;     (define-key omnisharp-mode-map (kbd "C-o") 'omnisharp-go-to-definition)
;;     (define-key omnisharp-mode-map (kbd "C-i") 'omnisharp-current-type-documentation)
;;     ;; (define-key omnisharp-mode-map (kbd "<tab>") 'omnisharp-auto-complete)
;;     )

(use-package js-auto-beautify
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'js-auto-beautify-mode)
  )

;; pip install flake8
;; (use-package flymake-python-pyflakes
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;;   ;; (setq flymake-python-pyflakes-executable "flake8")
;;   (setq flymake-python-pyflakes-extra-arguments '("--ignore=C0111"))
;;   )

(use-package powerline
  :ensure t)

;; (use-package pdf-tools
;;   :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )

;; (use-package multi-web-mode
;;   :ensure t
;;   :config
;;   (setq mweb-default-major-mode 'html-mode)
;;   (setq mweb-tags 
;;      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;        (js-mode  "<script[^>]*>" "</script>")
;;        (css-mode "<style[^>]*>" "</style>")))
;;   (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;   (multi-web-global-mode 1))

;; (use-package skewer-mode
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'skewer-mode)
;;   (add-hook 'css-mode-hook 'skewer-css-mode)
;;   (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package swiper
  :ensure t)

(use-package atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

(use-package which-key
  ;; displays available keybindings in popup
  :ensure t
  :config
  (which-key-mode)
  )

;; Let Emacs move the cursor off-screen
(use-package scroll-restore
  :ensure t
  :config
  (setq scroll-restore-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t) ;; or doom-dark, etc.
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed
  ;; (doom-themes-org-config)
  )

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  )

;; M-x customize-face RET auto-dim-other-buffers-face RET #333843
;;(use-package auto-dim-other-buffers
  ;; :config
  ;; (auto-dim-other-buffers-mode t)
  ;; )

;; Since I just was bitten by this. Installation of the fonts is as simple as:
;; $ git clone https://github.com/domtronn/all-the-icons.el.git
;; $ install -m 0644 -D all-the-icons.el/fonts/*.ttf -t ~/.local/share/fonts/
(use-package all-the-icons
  :ensure t
  :init 
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))
  :config
  (all-the-icons-octicon "file-binary")
  )

;;(use-package solaire-mode
;;  :ensure t
;;  :config
;;  ;; brighten buffers (that represent real files)
;;  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
;;  ;; To enable solaire-mode unconditionally for certain modes:
;;  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;;
;;  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;;  ;; itself off every time Emacs reverts the file
;;  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
;;
;;  ;; highlight the minibuffer when it is activated:
;;  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
;;
;;  ;; if the bright and dark background colors are the wrong way around, use this
;;  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;;  ;; This should be used *after* you load the active theme!
;;  ;;
;;  ;; NOTE: This is necessary for themes in the doom-themes package!
;;  (solaire-mode-swap-bg)
;;  )

;; (use-package linum-relative
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-x i") 'linum-relative-toggle))


;; automatic and manual symbol highlighting for Emacs
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'text-mode-hook 'highlight-symbol-mode)
  (global-unset-key (kbd "C-q"))
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key (kbd "C-q") 'highlight-symbol-next)
  (global-set-key (kbd "C-S-q") 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )

(use-package right-click-context
  :ensure t
  :quelpa (right-click-context :fetcher github :repo "zonuexe/right-click-context")
  :config
  (right-click-context-mode 1)
  )

(use-package org
  :ensure t
  :config
  ;; (setq org-log-done 'time)

  (define-key org-mode-map (kbd "C-c l") 'org-scontexttore-link)
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-h") 'delete-backward-char)
  (define-key org-mode-map (kbd "M-h") 'backward-delete-word)

  (define-key org-mode-map (kbd "C-i") 'org-shiftright)
  (define-key org-mode-map (kbd "C-S-i") 'org-shiftleft)

  (define-key org-mode-map (kbd "C-o") 'org-metaright)
  (define-key org-mode-map (kbd "C-S-o") 'org-metaleft)

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
  )

(use-package org-super-agenda
  :ensure t
  :quelpa (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda")
  :config
  (org-super-agenda-mode)
  )

;;(use-package workgroups2
;;  :ensure t
;;  :config
;;  (setq wg-session-file (concat settings_path "emacsd/.emacs_workgroups"))
;;  (workgroups-mode 1)
;;  (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
;;  (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
;;
;;  ;; Set your own keyboard shortcuts to reload/save/switch WGs:
;;  ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
;;  (global-set-key (kbd "<pause>") 'wg-reload-session)
;;  (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
;;  (global-set-key (kbd "s-z") 'wg-switch-to-workgroup)
;;  (global-set-key (kbd "s-\\") 'wg-switch-to-previous-workgroup)
;;  )

;; Don’t open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

(ac-config-default)

;; Charging isplay
;; (add-hook 'after-init-hook #'fancy-battery-mode)
(setq display-battery-mode 1)

;; (add-hook 'after-init-hook 'session-initialize)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
;; (editorconfig-mode 1)

;; Editor settings
;; (global-linum-mode)
(global-set-key (kbd "C-c i") `linum-mode)
;; (global-set-key (kbd "C-c a") `auto-revert-mode)
(electric-pair-mode 1)
;; Column marker

(setq fci-rule-width 1)
(setq fci-rule-color "dim gray")
;; show-paren-mode allows one to see matching pairs of parentheses and other characters.
(global-set-key (kbd "C-c u") `show-paren-mode)

;; Emcas help/dev
(global-set-key (kbd "C-c C-f") 'find-function)
;; (global-set-key (kbd "C-c j") 'describe-function)
;; (global-set-key (kbd "C-c k") 'describe-key)

;; Word wrap
(visual-line-mode 1)
;; (global-set-key (kbd "C-c b") 'visual-line-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq desktop-load-locked-desktop t)
(call-interactively 'desktop-read t (vector "~/.emacs.d/" t))

;; Make sure that your Emacs was compiled with module support.
;; Check that module-file-suffix is not nil
;; --> (message module-file-suffix)

;; (load (concat settings_path "parsers/target/debug/libparsers.so"))

(load-file (concat settings_path "functions.el"))
(load-file (concat settings_path "menu.el"))

(define-key lisp-mode-map (kbd "C-i") 'describe-function-in-popup)
