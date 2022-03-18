(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq load-path
      (append (delete-dups load-path)
              '("~/.emacs.d/lisp")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Use UTF-8 all the time
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun nossralf/macos ()
  "Run macOS-specific setup."
  ;; More reasonable scroll behavior
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

(defun nossralf/linux/x ()
  "Run Linux-specific setup."
  (set-face-attribute 'default nil :height 90))

(defun nossralf/linux/emacsclient-frame-hook (frame)
  "A hook to set an emacsclient-created FRAME to be maximized and have the correct face attribute settings."
  (when (display-graphic-p frame)
    (select-frame frame)
    (set-face-attribute 'default nil :height 90)))

;; Perform platform-specific setup
(cond
 ((and (string= system-type "gnu/linux") (daemonp))
  (add-hook 'after-make-frame-functions
            'nossralf/linux/emacsclient-frame-hook))
 ((memq window-system '(x))
  (nossralf/linux/x))
 ((memq window-system '(mac ns))
  (nossralf/macos)))

(electric-pair-mode t)
(global-hl-line-mode)
(delete-selection-mode t)
(global-whitespace-mode)

(if (version< emacs-version "26.1")
    (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'eldoc-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Faster window actions
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)

;; --- Modes ---

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package better-defaults)

(use-package company)

(use-package ctrlf
  :config
  (ctrlf-mode t))

(use-package dockerfile-mode
  :commands dockerfile-mode)

(use-package erlang
  :commands erlang-mode
  :bind (:map erlang-mode-map
              ("C-c e" . nossralf/erlang/export-current-function)))

(use-package nossralf-erlang
  :straight nil
  :after erlang)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package fish-mode
  :commands fish-mode)

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package forge
  :after magit)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (add-hook 'before-save-hook 'lsp-format-buffer nil 'local)
              (add-hook 'before-save-hook 'lsp-organize-imports nil 'local))))

(use-package groovy-mode
  :commands groovy-mode)

(use-package hardcore-mode
  :config
  (global-hardcore-mode))

(use-package git-commit
  :defer t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package helm
  :bind (("C-x C-b" . helm-buffers-list)
         ("C-x C-d" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-x" . helm-M-x)
         ("C-c C-x" . execute-extended-command)
         ("C-c o" . helm-occur))
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)
  (helm-mode))

(use-package helm-flycheck
  :config
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-rg
  :defer t)

(use-package lsp-mode
  :hook ((rust-mode . lsp-deferred) (go-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (setq read-process-output-max (* 1024 1024)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package magit
  :bind (("M-g b" . magit-blame)
         ("M-g d" . magit-diff)
         ("M-g l" . magit-log)
         ("M-g s" . magit-status)))

(use-package markdown-mode
  :mode  "\\.md\\'")

(use-package move-text
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package neotree
  :bind (("C-c d" . neotree-toggle)))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-{" . paredit-wrap-curly)
              ("M-[" . paredit-wrap-square))
  :hook (emacs-lisp-mode . paredit-mode))

(use-package popwin
  :defer t
  :config
  (popwin-mode 1))

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode t))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package puppet-mode
  :commands puppet-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rjsx-mode
  :commands rjsx-mode)

(defun nossralf/read-rust-src-path (toolchain)
  "Figure out the source code location for the given Rust TOOLCHAIN."
  (let ((cmd (format "rustc +%s --print sysroot" (symbol-name toolchain))))
    (concat (string-trim (shell-command-to-string cmd)) "/lib/rustlib/src/rust/src")))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'rust-format-buffer nil 'local))))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package subword-mode
  :straight (:type built-in)
  :hook (go-mode python-mode rust-mode))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("s-i" . symbol-overlay-put)))

(use-package systemd)

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package unfill
  :bind (("M-Q" . unfill-paragraph)))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode
  :commands yaml-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
