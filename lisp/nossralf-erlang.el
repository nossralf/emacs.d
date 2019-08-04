(require 'erlang)

(defun nossralf/erlang/export-current-function ()
  "Add the current Erlang function to the module's `export'
attribute."
  (interactive)
  (save-excursion
    (erlang-beginning-of-function)
    (let ((fun (erlang-get-function-name))
          (arity (erlang-get-function-arity)))
      (unless (erlang-function-exported-p fun arity)
        (nossralf/erlang/add-function-to-export-attribute fun arity)))))

(defun nossralf/erlang/add-function-to-export-attribute (fun arity)
  (let ((exports (mapcar (lambda (c) (format "%s/%d" (car c) (cdr c)))
                         (append (erlang-get-export) (list (cons fun arity))))))
    (nossralf/erlang/replace-export-attribute (nossralf/erlang/build-export-attribute (sort exports 'string-lessp)))))

(defun nossralf/erlang/build-export-attribute (function-signatures)
  "Create an export module attribute for the given
FUNCTION-SIGNATURES."
  (format "-export([%s])." (string-join function-signatures ", ")))

(defun nossralf/erlang/replace-export-attribute (new-export-attribute)
  "Replace the export module attribute in the buffer with
NEW-EXPORT-ATTRIBUTE."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "-export(\\[\\(.*?\\)\\]).$")
      (replace-match new-export-attribute))))

(provide 'nossralf-erlang)
