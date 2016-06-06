;;; hrm-company-mode.el --- Completion for Human Resource Machine programs.

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/hrm-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;;; Commentary:

;;; Code:

(require 'hrm-mode)
(require 'company)

(defun hrm-company-mode-backend (command &optional arg &rest ignored)
  "Human Resource Machine Company backend.

COMMAND is the completion command, ARG is its argument, IGNORED is ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'hrm-company-mode-backend))
    (prefix (company-grab-symbol))
    (candidates (when (equal arg "foo")
                  (list "foobar" "foobaz" "foobarbaz")))
    (meta (format "This value is named %s" arg))))

;; (add-hook hrm-mode-hook

(provide 'hrm-company-mode)
;;; hrm-company-mode.el ends here
