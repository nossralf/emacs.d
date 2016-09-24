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

;; Use UTF-8 all the time
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq ring-bell-function 'ignore)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun nossralf/osx-specific ()
  "OS X-specific setup"
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
  ;; More reasonable scroll behavior
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
  ;; Rejigger the keys to my liking
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(defun nossralf/linux-specific ()
  "Linux-specific setup"
  ;; For some reason the font size on Linux is rather huge. Make it smaller.
  (set-face-attribute 'default nil :height 90))

;; Perform platform-specific setup
(cond
 ((memq window-system '(x)) (nossralf/linux-specific))
 ((memq window-system '(mac ns)) (nossralf/osx-specific)))

(electric-pair-mode t)
(global-hl-line-mode)
(delete-selection-mode t)
(global-whitespace-mode)

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

(global-set-key (kbd "C-c l p") 'list-packages)

;; --- Modes ---

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

(use-package better-defaults
  :ensure t)

(use-package cargo
  :ensure t)

(use-package company
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-rust
  :ensure t)

(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package helm
  :ensure t
  :bind (("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-x" . helm-M-x)
         ("C-c C-x" . execute-extended-command))
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)
  (helm-mode))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :bind (("C-c o" . helm-swoop)))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package magit
  :ensure t)

(use-package move-text
  :ensure t
  :bind (("M-P" . move-text-up)
         ("M-N" . move-text-down)))

(use-package neotree
  :ensure t
  :bind (("C-c d" . neotree-toggle)))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("M-{" . paredit-wrap-curly)
              ("M-[" . paredit-wrap-square))
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package racer
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq racer-rust-src-path "~/.rust-source/stable/src")
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'rustfmt-format-buffer nil 'local))))

(use-package rustfmt
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package toml-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :ensure t)
