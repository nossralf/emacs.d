(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Use UTF-8 all the time
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

(use-package emacs
  :straight (:type built-in)
  :hook ((after-init . electric-pair-mode)
         (after-init . delete-selection-mode)
         (after-init . global-hl-line-mode)
         (after-init . global-whitespace-mode)
         (before-save . delete-trailing-whitespace)
         (prog-mode . column-number-mode)
         (prog-mode . eldoc-mode)
         (prog-mode . display-line-numbers-mode))
  :bind (("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right)
         ("M-0" . delete-window)
         :map indent-rigidly-map
         ("s-j" . indent-rigidly-left)
         ("s-k" . indent-rigidly-right)))

;; --- Modes ---

(use-package ace-window
  :bind (("s-o" . ace-window))
  :config (set-face-attribute 'aw-leading-char-face nil :height 300))

(use-package adoc-mode)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package ansible-mode
  :straight nil
  :config
  (defun maybe-ansible-mode ()
    (if (ansible-project-p) (ansible-mode)))
  (add-hook 'yaml-mode-hook 'maybe-ansible-mode))

(use-package apheleia
  :hook (prog-mode . apheleia-mode))

(use-package auto-fill-mode
  :straight (:type built-in)
  :hook (adoc-mode markdown-mode org-mode))

(use-package avy
  :bind (("s-l" . avy-goto-char-timer)))

(use-package bazel)

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ([remap indent-for-tab-command] . company-indent-or-complete-common)))

(use-package counsel
  :after ivy
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x f" . counsel-recentf)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x b" . counsel-switch-buffer)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)))

(use-package crux
  :bind (("C-k" . crux-smart-kill-line)))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package dockerfile-mode
  :commands dockerfile-mode)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package erlang
  :commands erlang-mode
  :bind (:map erlang-mode-map
              ("C-c e" . nossralf/erlang/export-current-function)))

(use-package nossralf-erlang
  :straight nil
  :after erlang)

(use-package esup
  :commands esup
  :config
  (setq esup-depth 0))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package fish-mode
  :commands fish-mode)

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package forge
  :after magit)

(use-package go-mode
  :commands go-mode
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

(use-package hcl-mode)

(use-package htmlize)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package jinx
  :hook ((after-init . global-jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("s-:" . jinx-correct-all))
  :config
  (add-to-list 'jinx-include-faces
               '(prog-mode font-lock-comment-face
                           font-lock-doc-face
                           font-lock-string-face
                           tree-sitter-hl-face:comment
                           tree-sitter-hl-face:string)))

(use-package kotlin-ts-mode
  :mode "\\.kts?\\'")

(use-package lsp-java
  :hook (java-mode . lsp-deferred))

;; As the lsp function checks for "ansible", we need to redefine it and instead
;; check for "ansible-mode". This redefinition can replace the original
;; function via `advice-add'.
(defun nossralf/lsp-ansible-check-ansible-minor-mode (&rest _)
  "Check whether ansible minor mode is active.
This prevents the Ansible server from being turned on in all yaml files."
  (and (or (derived-mode-p 'yaml-mode)
           (derived-mode-p 'yaml-ts-mode))
       ;; emacs-ansible provides ansible, not ansible-mode
       (with-no-warnings (bound-and-true-p ansible-mode))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook  ((go-mode . lsp-deferred)
          (powershell-mode . lsp-deferred)
          (c-mode . lsp-deferred)
          (ansible-mode . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration))
  :bind (("s-." . lsp-execute-code-action))
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (advice-add 'lsp-ansible-check-ansible-minor-mode :override #'nossralf/lsp-ansible-check-ansible-minor-mode))

(use-package lsp-booster
  :straight nil
  :after lsp-mode
  :config (lsp-booster-mode))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lua-mode)

(use-package magit
  :bind (("M-g b" . magit-blame)
         ("M-g d" . magit-diff)
         ("M-g l" . magit-log)
         ("M-g s" . magit-status))
  :hook (git-commit-mode . flyspell-mode)
  :init
  (if (eq window-system 'mac) (advice-add 'with-editor-locate-emacsclient :override (lambda () "Always return nil." nil))))

(use-package markdown-mode
  :mode  "\\.md\\'")

(use-package move-text
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package neotree
  :bind (("C-c d" . neotree-toggle)))

(use-package org)

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-{" . paredit-wrap-curly)
              ("M-[" . paredit-wrap-square))
  :hook (emacs-lisp-mode . paredit-mode))

(use-package popwin
  :defer t
  :config
  (popwin-mode 1))

(use-package powershell
  :hook (before-save . lsp-format-buffer))

(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :init
  (projectile-mode +1))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package puppet-mode
  :commands puppet-mode)

(use-package python-isort
  :hook (python-mode . python-isort-on-save-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rg)

(use-package rjsx-mode
  :commands rjsx-mode)

(use-package rustic
  :commands rustic-mode)

(use-package save-place-mode
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package savehist-mode
  :straight (:type built-in)
  :hook (after-init . savehist-mode))

(use-package show-paren-mode
  :straight (:type built-in)
  :hook (after-init . show-paren-mode))

(use-package subword-mode
  :straight (:type built-in)
  :hook (go-mode powershell-mode python-mode rust-mode))

(use-package swiper
  :bind (("C-s" . swiper-isearch)))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("s-i" . symbol-overlay-put)))

(use-package systemd)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package unfill
  :bind (("M-Q" . unfill-paragraph)))

(use-package vala-mode)

(use-package web-mode)

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode
  :commands yaml-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :defer t)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
