;;; lsp-booster.el --- Boost lsp-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses emacs-lsp-booster to improve the performance of lsp-mode.

;;; Code:

(require 'lsp-mode)

(defcustom lsp-booster-io-only nil
  "If non-nil, do not translate JSON to bytecode."
  :group 'lsp-mode
  :type 'boolean)

(defvar lsp-booster--boost-io-only
  '("emacs-lsp-booster" "--disable-bytecode" "--"))

(defvar lsp-booster--boost
  '("emacs-lsp-booster" "--"))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD and wrap OLD-FN."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (if lsp-booster-io-only
              (append lsp-booster--boost-io-only orig-result)
            (append lsp-booster--boost orig-result)))
      orig-result)))

;;;###autoload
(define-minor-mode lsp-booster-mode
  "Boosts lsp-mode."
  :global t
  :group lsp-mode
  (cond (lsp-booster-mode
         (unless (executable-find "emacs-lsp-booster")
           (setq lsp-booster-mode nil)
           (user-error "The emacs-lsp-booster program is not installed"))
         (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
        (t
         (advice-remove 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))))

(provide 'lsp-booster)
;;; lsp-booster.el ends here
