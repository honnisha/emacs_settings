(load-file (concat settings_path "functions.el"))

;; sudo apt-get install pylint
;; sudo pip install flake8

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

;; Automatically save and restore sessions
(desktop-save-mode 1)

;; Finally you can toggle the display of scroll bars on all frames
(scroll-bar-mode -1)

;; if you want it for every buffer
(global-linum-mode t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

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
(setq history-delete-duplicates t)

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

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 85 :width normal)))))

(global-set-key (kbd "C-x w") (lambda() (interactive)(eww "google.com")))

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
(global-set-key (kbd "C-x n !") (lambda() (interactive)(find-file (concat dropbox_path "org_files/main.org"))))
(global-set-key (kbd "C-x n @") (lambda() (interactive)(find-file (concat dropbox_path "org_files/work.org"))))

(global-set-key (kbd "C-c h") `whitespace-mode)

(global-set-key (kbd "C-x c") `list-colors-display)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "C-c r") `revert-buffer)
(global-set-key (kbd "C-r") `replace-string)

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

;; Org mode
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c l") 'org-store-link)
             (define-key org-mode-map (kbd "C-c a") 'org-agenda)
	     ))

;; Windows/frames
(global-set-key (kbd "<C-S-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)

(global-set-key (kbd "<C-S-kp-right>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-kp-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-kp-down>") 'shrink-window)
(global-set-key (kbd "<C-S-kp-up>") 'enlarge-window)
;; Emacs help/dev
(global-set-key (kbd "C-c C-f") 'find-function)


(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice)
  )

(use-package window-number
  :ensure t
  :quelpa (window-number :fetcher github :repo "nikolas/window-number")
  )

(use-package smartparens
  :ensure t
  :config
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'shell-mode-hook #'smartparens-mode)
  )

(use-package dumb-jump
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'dumb-jump-go)
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

  '(tabbar-ruler-excluded-buffers
   (quote
    ("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*" "*helm-mini*" "*helm-mode-describe-variable*" "*anaconda-mode*" "*Anaconda*" "*Compile-Log*")))
  )

(window-number-meta-mode)
(window-number-mode 1)

;; Available only on mercurial versions 1.9 or higher
(setq monky-process-type 'cmdserver)
(global-set-key (kbd "C-c l") 'monky-status)

;; centered-cursor-mode
(global-set-key (kbd "<Scroll_Lock>") 'centered-cursor-mode)
(global-set-key (kbd "<C-Scroll_Lock>") 'scroll-lock-mode)

;; Neotree
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

(require 'use-package)
;; (require 'multiple-cursors)
;; (require 'google-translate)
;; (require 'google-translate-default-ui)

;; Install the Git frontend Magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'magit-status)
  (global-set-key (kbd "C-c C-m") 'magit-dispatch-popup)
  )

(use-package org-jira
  :ensure t
  :config
  (setq jiralib-url "https://recallmasters.atlassian.net")
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

;; Rust
;; rustup component add rust-src
(use-package rust-mode
  :ensure t
  :config
  ;; cargo install racer
  (setq racer-cmd "~/.cargo/bin/racer")
  ;;rustc --print sysroot /lib/rustlib/src/rust/src
  (setq racer-rust-src-path "/home/gangashman/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

  ;; Configure Emacs to activate racer when rust-mode starts
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (define-key rust-mode-map (kbd "C-o") #'racer-find-definition)
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

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist '(("en" . "ru")))
  (global-set-key "\C-ct" 'google-translate-smooth-translate))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(use-package neotree
  :ensure t)

(use-package racer
  :ensure t)

;; (use-package restclient
;;   :ensure t)

;; https://github.com/emacs-pe/company-racer
(use-package company-racer
  :ensure t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer))
  )

;; (use-package yascroll
;;   :ensure t
;;   :config
;;   (global-yascroll-bar-mode 1))

(use-package cargo
  :ensure t)

;;(use-package flycheck-rust
;;  :ensure t
;;  :config
;;  (add-hook 'flycheck-mode-hook #'flycheck-rust-metup))

;; (use-package ess
;;   :ensure t)

(use-package helm
  :ensure t
  :init
  (setq helm-mode t)
  :config
  (setq helm-split-window-in-side-p t)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
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
  )

(use-package helm-projectile
  :ensure t
  :config
  (global-set-key (kbd "C-x s") 'helm-projectile-grep)
  (global-unset-key (kbd "C-M-j"))
  (global-set-key (kbd "C-M-j") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-SPC") 'helm-projectile-switch-to-buffer)
  )

;;(use-package elpy
;;  :ensure t
;;  :config
;;  (elpy-enable))

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
  )

(use-package company
  :ensure t
  :config
  (global-company-mode))

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
  )

(use-package flycheck-julia
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

;; pip install isort
;; https://github.com/timothycrosley/isort
(use-package py-isort
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'py-isort-buffer)
  )

(use-package epc
  :ensure t)

(use-package format-sql
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode t))

;; Python

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
;; 	))
;;   )

(use-package anaconda-mode
  :ensure t
  :config
  (define-key python-mode-map (kbd "C-o") 'anaconda-mode-find-definitions)
  (define-key python-mode-map (kbd "C-i") 'anaconda-mode-show-doc)
  (define-key python-mode-map (kbd "C-c a") 'pythonic-activate)

  (define-key python-mode-map (kbd "<tab>") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "<backtab>") 'python-indent-shift-left)

  (add-to-list 'python-shell-extra-pythonpaths "~/.virtualenvs/gc")
  (add-hook 'python-mode-hook 'anaconda-mode)
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

;; This requires pyflakes to be on PATH. Alternatively, set pyimport-pyflakes-path.
(use-package pyimport
  :ensure t
  :config
  (define-key jedi-mode-map (kbd "C-c C-o") 'pyimport-remove-unused)
  (define-key jedi-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
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

;; (use-package anaconda-mode
;;   :ensure t
;;   :config
;;   (add-hook 'ropemacs-mode-hook 'anaconda-mode))

;; pip install flake8
(use-package flymake-python-pyflakes
  :ensure t
  :config
  (add-hook 'ropemacs-mode-hook 'flymake-python-pyflakes-load)
  (setq flymake-python-pyflakes-executable "flake8")
  (setq flymake-python-pyflakes-extra-arguments '("--ignore=C0111")))

(use-package powerline
  :ensure t)

;; (use-package pdf-tools
;;   :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; (use-package multi-web-mode
;;   :ensure t
;;   :config
;;   (setq mweb-default-major-mode 'html-mode)
;;   (setq mweb-tags 
;; 	'((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;; 	  (js-mode  "<script[^>]*>" "</script>")
;; 	  (css-mode "<style[^>]*>" "</style>")))
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

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1))

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

(use-package linum-relative
  :ensure t
  :config
  (global-set-key (kbd "C-x i") 'linum-relative-toggle))


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

(use-package org
  :ensure t)

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
(auto-complete-mode)

;; Charging isplay
;; (add-hook 'after-init-hook #'fancy-battery-mode)
(setq display-battery-mode 1)

;; (add-hook 'after-init-hook 'session-initialize)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
;; (editorconfig-mode 1)
;; Error navigation
(global-set-key (kbd "M-z") 'flycheck-previous-error)
(global-set-key (kbd "C-z") 'flycheck-next-error)
(global-set-key (kbd "C-M-z") 'flycheck-copy-errors-as-kill)

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
