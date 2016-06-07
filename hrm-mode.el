;;; hrm-mode.el --- Major mode for Human Resource Machine programs.

;; Version: 0.1.0
;; Author: Vladimir Panteleev
;; Url: https://github.com/CyberShadow/hrm-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;;; Commentary:

;; A Major Emacs mode for editing Human Resource Machine programs.

;; Spoiler warning: contains a full list of instructions.
;; (Additional instructions are discovered as the game progresses.)

;;; Code:

(require 'generic)

(defgroup hrm-mode nil
  "Major mode for editing Human Resource Machine programs."
  :link '(url-link "https://tomorrowcorporation.com/humanresourcemachine")
  :group 'languages)

(defface hrm-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Font for comments."
  :group 'hrm-mode)

(defface hrm-mode-instruction-face
  '((t :inherit font-lock-keyword-face))
  "Font for instruction opcodes."
  :group 'hrm-mode)

(defface hrm-mode-number-face
  '((t :inherit default))
  "Font for numbers."
  :group 'hrm-mode)

(defface hrm-mode-punctuation-face
  '((t :inherit font-lock-builtin-face))
  "Font for punctuation characters."
  :group 'hrm-mode)

(defface hrm-mode-label-face
  '((t :inherit font-lock-constant-face))
  "Font for labels."
  :group 'hrm-mode)

(defface hrm-mode-definition-face
  '((t :inherit hrm-mode-instruction-face))
  "Font for keywords defining labels."
  :group 'hrm-mode)

(defface hrm-mode-data-face
  '((t :inherit font-lock-string-face))
  "Font for label data."
  :group 'hrm-mode)

(defface hrm-mode-error-face
  '((t :inherit font-lock-warning-face))
  "Font for invalid syntax."
  :group 'hrm-mode)

(defconst hrm-mode-re-label-name "\\([a-zA-Z][a-zA-Z0-9]*\\)")

(defconst hrm-mode-re-label (concat "^\\s-*" hrm-mode-re-label-name "\\(:\\)"))

(defconst hrm-mode-re-op-jump (concat "^\\s-*\\(JUMP\\|JUMPZ\\|JUMPN\\)\\s-+" hrm-mode-re-label-name))

(defvar hrm-mode-font-lock-keywords)
(setq hrm-mode-font-lock-keywords
  `(
    ;; Comments
    ("--.*$" . 'hrm-mode-comment-face)

    ;; Basic instructions (without arguments)
    ("^\\s-*\\(INBOX\\|OUTBOX\\)"
     (1 'hrm-mode-instruction-face)
     )
    
    ;; Register instructions (with numeric argument)
    ("^\\s-*\\(COPYFROM\\|COPYTO\\|ADD\\|SUB\\|BUMPUP\\|BUMPDN\\|COMMENT\\)\\s-+\\([0-9]+\\)"
     (1 'hrm-mode-instruction-face)
     (2 'hrm-mode-number-face)
     )
    
    ;; Register instructions (with indirect numeric argument)
    ("^\\s-*\\(COPYFROM\\|COPYTO\\|ADD\\|SUB\\|BUMPUP\\|BUMPDN\\|COMMENT\\)\\s-+\\(\\[\\)\\([0-9]+\\)\\(\\]\\)"
     (1 'hrm-mode-instruction-face)
     (2 'hrm-mode-punctuation-face)
     (3 'hrm-mode-number-face)
     (4 'hrm-mode-punctuation-face)
     )
    
    ;; Jump instructions (with label argument)
    (,hrm-mode-re-op-jump
     (1 'hrm-mode-instruction-face)
     (2 'hrm-mode-label-face)
     )
    
    ;; Jump labels
    (,hrm-mode-re-label
     (1 'hrm-mode-label-face)
     (2 'hrm-mode-punctuation-face)
     )

    ;; Drawn labels
    ("^\\s-*\\(DEFINE \\(COMMENT\\|LABEL\\)\\) \\([0-9]+\\)$"
     (1 'hrm-mode-definition-face)
     (3 'hrm-mode-number-face t)
     ("[a-zA-Z0-9/+]"
      (hrm-mode-end-of-data)
      nil
      (0 'hrm-mode-data-face)
      )
     (";$"
      (hrm-mode-end-of-data)
      nil
      (0 'hrm-mode-punctuation-face)
      )
     )

    ;; Anything else is an error
    ("[^ \n]" . 'hrm-mode-error-face)))

;;;###autoload
(define-derived-mode hrm-mode prog-mode "HRM"
  "Major mode for editing Human Resource Machine programs."

  :group 'hrm-mode

  ;; Settings
  (setq-local indent-line-function 'hrm-mode-indent-function)
  (setq-local font-lock-multiline t)
  (setq-local imenu-generic-expression
	'(("Label" "^\\(.*\\):" 1)
	  ))
  (setq-local case-fold-search nil)

  ;; Comments
  (setq-local comment-start "-- ")
  (setq-local comment-end   "")

  ;; Syntax
  (setq-local font-lock-defaults '(hrm-mode-font-lock-keywords
                                   nil nil nil nil)))

(defun hrm-mode-end-of-data (&rest foo)
  "Return position of the end of data blocks.  FOO is ignored."
  (save-excursion
    (search-forward ";" nil 1)
    (forward-char)
    (point)))

(defconst hrm-mode-opcodes
  '("INBOX" "OUTBOX" "COPYFROM" "COPYTO" "ADD" "SUB" "BUMPUP" "BUMPDN" "JUMP" "JUMPZ" "JUMPN" "COMMENT"))
(defconst hrm-mode-opcodes-regex
  (concat "\\(" (mapconcat 'identity hrm-mode-opcodes "\\|") "\\)"))

(require 'thingatpt)
(defun hrm-mode-indent-function ()
  "Human Resource Machine indent rule."
  (save-excursion
    (back-to-indentation)
    (let ((w (word-at-point)))
      ;; (message w)
      (if (looking-at hrm-mode-opcodes-regex)
	  (indent-line-to 4)
	(indent-line-to 0)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hrm\\'" . hrm-mode))

(provide 'hrm-mode)
;;; hrm-mode.el ends here
