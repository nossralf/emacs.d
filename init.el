(require 'package)

(add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun nossralf/osx ()
  (let ((px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty)
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'default-frame-alist (cons 'width tx))
    (add-to-list 'default-frame-alist (cons 'height ty)))
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))
(if (memq window-system '(mac ns)) (nossralf/osx))

(electric-pair-mode t)
(global-hl-line-mode)
(delete-selection-mode t)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'eldoc-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq inhibit-startup-message t)
(setq visible-bell t)

(set-frame-font "Inconsolata-g-11")

;; Faster window actions
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)

;; --- Modes ---

(use-package better-defaults
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x f") 'helm-recentf)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode))

(use-package racer
  :ensure t)

(use-package company
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq racer-rust-src-path "/Users/nossralf/.rust-source/stable/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package yaml-mode
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'neotree-toggle))

(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))
