;;; ansible-mode.el --- Ansible minor mode -*- lexical-binding: t -*-

;;; Commentary:

;; A minimalist minor mode for Ansible.

;;; Code:

(require 'cl-lib)

(defgroup ansible-mode nil
  "Ansible mode."
  :group 'ansible-mode
  :tag "Ansible mode")

(defcustom ansible-mode-project-dominating-files '("ansible.cfg" ".ansible-lint")
  "File names that determine if a file is in an Ansible project."
  :group 'ansible-mode
  :type '(repeat string))

(define-minor-mode ansible-mode
  "A minor mode for Ansible."
  :lighter " ansible")

(defun ansible-project-p ()
  "Return non-nil if current buffer is part of an Ansible project."
  (cl-loop for f in ansible-mode-project-dominating-files
           if (locate-dominating-file default-directory f)
           return t))

(provide 'ansible-mode)
;;; ansible-mode.el ends here
