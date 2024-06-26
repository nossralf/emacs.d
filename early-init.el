(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq package-enable-at-startup nil
      frame-title-format nil)

;; Hard-code locale to en_US.UTF-8 because my language settings in macOS end up
;; as en_SE.UTF-8 which doesn't exist in /usr/share/locale and that causes
;; issues when using ansible-language-server with lsp-mode.
(setenv "LANG" "en_US.UTF-8")
