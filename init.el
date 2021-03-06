(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq load-path
      (append (delete-dups load-path)
              '("~/.emacs.d/lisp")))

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Use UTF-8 all the time
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun nossralf/macos ()
  "Run macOS-specific setup."
  ;; Maximize window
  (toggle-frame-maximized)
  ;; More reasonable scroll behavior
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  ;; Rejigger the keys to my liking
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(defun nossralf/linux/x ()
  "Run Linux-specific setup."
  (toggle-frame-maximized)
  (set-face-attribute 'default nil :height 90))

(defun nossralf/linux/emacsclient-frame-hook (frame)
  "A hook to set an emacsclient-created FRAME to be maximized and have the correct face attribute settings."
  (when (display-graphic-p frame)
    (select-frame frame)
    (toggle-frame-maximized)
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

(global-set-key (kbd "C-c l p") 'list-packages)

;; --- Modes ---

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

(use-package better-defaults
  :ensure t)

(use-package cargo
  :ensure t
  :defer t)

(use-package company
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package erlang
  :ensure t
  :defer t
  :bind (:map erlang-mode-map
              ("C-c e" . nossralf/erlang/export-current-function)))

(use-package nossralf-erlang
  :after erlang)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-rust
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-topic-list-limit '(60 . 0)))

(use-package groovy-mode
  :ensure t
  :defer t)

(use-package hardcore-mode
  :ensure t
  :custom
  (too-hardcore-backspace t)
  (too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode))

(defun nossralf/topic-branch-commit-message ()
  "Make the commit message begin with the Jira issue for topic branches."
  (let* ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
         (found-topic-branch (string-match "On branch topic-\\([A-Z]+-[0-9]+\\)" buffer-contents))
         (issue-name (match-string 1 buffer-contents)))
    (when found-topic-branch
      (unless (string-prefix-p issue-name buffer-contents)
        (goto-char 0)
        (insert issue-name " ")))))

(use-package git-commit
  :ensure t
  :defer t
  :custom
  (git-commit-fill-column 72)
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-setup-hook 'nossralf/topic-branch-commit-message))

(use-package helm
  :ensure t
  :bind (("C-x C-b" . helm-buffers-list)
         ("C-x C-d" . helm-browse-project)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-x" . helm-M-x)
         ("C-c C-x" . execute-extended-command)
         ("C-c o" . helm-occur))
  :custom
  (helm-ff-keep-cached-candidates nil)
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)
  (helm-mode))

(use-package helm-flycheck
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-rg
  :ensure t
  :defer t)

(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode))

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :init
  (setq lsp-keymap-prefix "C-c j")
  (setq lsp-rust-server 'rust-analyzer)
  (setq read-process-output-max (* 1024 1024))
  :commands lsp)

(use-package magit
  :ensure t
  :bind (("M-g b" . magit-blame)
         ("M-g d" . magit-diff)
         ("M-g l" . magit-log)
         ("M-g s" . magit-status))
  :custom
  (magit-diff-refine-hunk t))

(use-package markdown-mode
  :ensure t
  :mode  "\\.md\\'"
  :custom
  (markdown-command "pandoc --from commonmark --to html5 --highlight-style pygments --standalone"))

(use-package move-text
  :ensure t
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package neotree
  :ensure t
  :bind (("C-c d" . neotree-toggle)))

(use-package nxml-mode
  :defer t
  :config
  (setq nxml-child-indent 4))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("M-{" . paredit-wrap-curly)
              ("M-[" . paredit-wrap-square))
  :hook (emacs-lisp-mode . paredit-mode))

(use-package popwin
  :ensure t
  :defer t
  :config
  (popwin-mode 1))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode t))

(use-package puppet-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rjsx-mode
  :ensure t
  :defer t
  :config
  (setq js2-basic-offset 2))

(defun nossralf/read-rust-src-path (toolchain)
  "Figure out the source code location for the given Rust TOOLCHAIN."
  (let ((cmd (format "rustc +%s --print sysroot" (symbol-name toolchain))))
    (concat (string-trim (shell-command-to-string cmd)) "/lib/rustlib/src/rust/src")))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'rust-format-buffer nil 'local))))

(use-package smart-mode-line
  :ensure t
  :defer t
  :custom
  (sml/theme 'respectful)
  :config
  (sml/setup))

(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
