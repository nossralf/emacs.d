(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)
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
  :bind (("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-x" . helm-M-x))
  :config
  (require 'helm-config)
  (helm-mode))

(use-package company
  :ensure t
  :pin melpa-stable)

(use-package racer
  :ensure t)

(use-package rustfmt
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq racer-rust-src-path "~/.rust-source/stable/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'before-save-hook #'rustfmt-format-buffer))

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
  :bind (("C-c d" . neotree-toggle)))

(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))
