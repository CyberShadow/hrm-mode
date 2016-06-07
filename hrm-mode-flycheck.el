;;; hrm-mode-flycheck.el --- Syntax check for Human Resource Machine programs.

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/hrm-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;;; Commentary:

;;; Code:

(require 'hrm-mode)
(require 'flycheck)

(defun hrm-mode-flycheck-start (checker callback)
  "Start a HRM syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  (let (errors)
    (save-excursion
      (goto-char (point-min))
      (let (pos)
	(while (setq pos (c-search-forward-char-property 'face 'hrm-mode-error-face))
	  (push (flycheck-error-new-at (line-number-at-pos) (current-column) 'error "Invalid syntax") errors)
	  (forward-line)))

      (let ((known-labels (hrm-mode-get-labels)))
	(goto-char (point-min))
	(while (search-forward-regexp hrm-mode-re-op-jump nil t)
	  (unless (member (match-string 2) known-labels)
	    (push (flycheck-error-new-at (line-number-at-pos) (current-column) 'error
					 (concat "Unknown label " (match-string 2))) errors)))))

    (funcall callback 'finished errors)))

(flycheck-define-generic-checker 'hrm-mode-flycheck
  "A syntax checker for Human Resource Machine programs."
  :start #'hrm-mode-flycheck-start
  :modes 'hrm-mode)

(add-to-list 'flycheck-checkers 'hrm-mode-flycheck)

(provide 'hrm-mode-flycheck)
;;; hrm-mode-flycheck.el ends here
