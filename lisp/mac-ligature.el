;;; mac-ligature.el --- Ligature support for macOS Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Mark Wunsch
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, ligatures
;; URL: https://github.com/yourusername/mac-ligature

;;; Commentary:

;; This package provides ligature support for macOS Emacs (and other Emacs
;; builds that don't have Harfbuzz support).  It is inspired by ligature.el
;; but uses composition-function-table directly, which works on macOS.
;;
;; Features:
;; - Fine-grained control over which ligatures display in which modes
;; - Pre-configured ligature sets for popular programming fonts
;; - Minor mode that can be enabled globally or per-buffer
;; - Works with prog-mode-hook for automatic enabling
;;
;; Usage:
;;
;;   ;; Set up ligatures for programming modes
;;   (require 'mac-ligature)
;;   (mac-ligature-set-ligatures 'prog-mode mac-ligature-prog-ligatures)
;;   (global-mac-ligature-mode 1)
;;
;; Or enable it via hook:
;;
;;   (add-hook 'prog-mode-hook #'mac-ligature-mode)

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Predefined Ligature Sets
;;; ============================================================================
;;; Defined early so they can be used in defcustom default values

(defconst mac-ligature-prog-ligatures
  '("--" "---" "&&" "||" "==" "===" "!=" "!==" ">=" "<="
    "&&" "||" "=>" "->>" "<<-" "->" "-<" ">-" ">>" "<<" "<<<"
    ">>>" "::" ":::" "..." "..<" "??" "?." "?:" "?="
    "++" "+++" "**" "***" "//" "///" "/*" "*/" "/**"
    "</" "</>" "/>" "<!--" "-->" "||>" "<||" "|>" "<|"
    "==>" "<==" "<=>" "<<=" ">>=" "<<<" ">>>"
    "<~" "~>" "~~" "~~>" "~@" "$>" "<$" "<+" "+>"
    "=<<" "=~" ">=" "<=")
  "A comprehensive set of ligatures for programming.
Suitable for most programming languages and fonts like JetBrains Mono,
Fira Code, and Cascadia Code.")

(defconst mac-ligature-jetbrains-mono-ligatures
  '("--" "---" "&&" "||" "==" "===" "!=" "!==" ">=" "<="
    "=>" "->" "->>" "<<-" "<-" "-<" ">-" ">>" "<<" ":::" "..."
    "++" "**" "//" "/*" "*/" "/**"
    "==>" "<==" "<=>" "||>" "<||" "|>" "<|"
    "<!--" "-->" "</" "</>" "/>" "!!" "??" "?." "?:"
    "~>" "~~>" "~@" "#[" "#!" "#?" "##" "###"
    "#{" "#=" "#!" "#:" "=:" "=!=" "=/=")
  "Ligatures supported by JetBrains Mono font.")

(defconst mac-ligature-fira-code-ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*")
  "Ligatures supported by Fira Code font.")

(defconst mac-ligature-cascadia-code-ligatures
  '("--" "---" "&&" "||" "==" "===" "!=" "!==" ">=" "<="
    "=>" "->" "<<" ">>" "::" ":::" "..."
    "++" "**" "//" "/*" "*/"
    "==>" "<==" "<=>" "||>" "|>" "<!--" "-->" "</" "/>"
    "!!" "??" "?." "##" "###" "#_" "~>" "~~>" "~@")
  "Ligatures supported by Cascadia Code font.")

(defconst mac-ligature-all-ligatures
  (delete-dups
   (append mac-ligature-prog-ligatures
           mac-ligature-jetbrains-mono-ligatures
           mac-ligature-fira-code-ligatures
           mac-ligature-cascadia-code-ligatures))
  "Comprehensive set of all known programming ligatures.
Combines ligatures from all predefined sets (prog, JetBrains Mono, Fira Code,
and Cascadia Code). Use this if you want maximum ligature coverage and your
font supports many ligatures.")

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup mac-ligature nil
  "Typographic ligatures for macOS Emacs."
  :group 'faces
  :prefix "mac-ligature-")

(defcustom mac-ligature-ligatures-for-modes
  (list (cons 'prog-mode mac-ligature-all-ligatures))
  "Alist of (MODE . LIGATURES) for mode-specific ligature configuration.

Each entry is a cons cell where:
- The car is a major mode symbol (e.g., 'prog-mode, 'python-mode)
  or a list of major mode symbols, or t for all modes
- The cdr is a list of ligature strings (e.g., '(\"->\" \"=>\" \"!=\"))

Example:
  ((prog-mode . (\"->\" \"=>\" \"!=\" \">=\" \"<=\"))
   (org-mode . (\"<!--\" \"-->\")))

You can also use predefined ligature sets:
  ((prog-mode . ,mac-ligature-all-ligatures))           ; All known ligatures (default)
  ((prog-mode . ,mac-ligature-jetbrains-mono-ligatures)) ; JetBrains Mono specific
  ((prog-mode . ,mac-ligature-fira-code-ligatures))      ; Fira Code specific
  ((prog-mode . ,mac-ligature-prog-ligatures))           ; Common subset

Changes to this variable take effect when `mac-ligature-mode' is
enabled or when you call `mac-ligature-set-ligatures'."
  :type '(alist :key-type (choice (symbol :tag "Mode")
                                   (repeat :tag "Modes" symbol)
                                   (const :tag "All modes" t))
                :value-type (repeat :tag "Ligatures" string))
  :group 'mac-ligature)

(defcustom mac-ligature-ignored-major-modes
  '(minibuffer-inactive-mode)
  "Major modes that should not have ligatures enabled."
  :type '(repeat symbol)
  :group 'mac-ligature)

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defvar-local mac-ligature--composition-table nil
  "Buffer-local composition function table for ligatures.")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun mac-ligature--normalize-modes (modes)
  "Normalize MODES to a list of mode symbols.
If MODES is t, return (list t).
If MODES is a symbol, return it as a single-element list.
If MODES is already a list, return it as-is."
  (cond
   ((eq modes t) (list t))
   ((symbolp modes) (list modes))
   ((listp modes) modes)
   (t (error "Invalid modes specification: %S" modes))))

(defun mac-ligature--group-by-char (ligatures)
  "Group LIGATURES by their first character.
Returns an alist of (CHAR . PATTERNS) where CHAR is the first character
code of each ligature and PATTERNS is a list of regex patterns for sequences
starting with that character."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (lig ligatures)
      (when (and (stringp lig) (> (length lig) 0))
        (let* ((first-char (aref lig 0))
               (pattern (regexp-quote lig)))
          (push pattern (gethash first-char groups)))))
    ;; Convert hash table to alist and build regex alternatives
    (let (result)
      (maphash (lambda (char patterns)
                 (push (cons char (string-join (nreverse patterns) "\\|"))
                       result))
               groups)
      (nreverse result))))

(defun mac-ligature--get-ligatures-for-mode (mode)
  "Get all ligatures that apply to MODE.
Combines ligatures from mode-specific entries and global (t) entries."
  (let (ligatures)
    (dolist (entry mac-ligature-ligatures-for-modes)
      (let ((modes (car entry))
            (ligs (cdr entry)))
        (when (and (not (memq major-mode mac-ligature-ignored-major-modes))
                   (or (eq modes t)
                       (and (listp modes) (memq t modes))
                       ;; Handle both single symbols and lists of symbols
                       (let ((mode-list (if (listp modes) modes (list modes))))
                         (apply #'derived-mode-p mode-list))))
          (setq ligatures (append ligatures ligs)))))
    (delete-dups ligatures)))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun mac-ligature-set-ligatures (modes ligatures)
  "Set LIGATURES for MODES.

MODES can be:
- A major mode symbol (e.g., 'prog-mode)
- A list of major mode symbols
- The symbol t (applies globally)

LIGATURES is a list of strings representing character sequences
to be rendered as ligatures.

This function modifies `mac-ligature-ligatures-for-modes' and
refreshes ligatures in all active buffers.

Example:
  (mac-ligature-set-ligatures 'prog-mode '(\"->\" \"=>\" \"!=\" \">=\" \"<=\"))
  (mac-ligature-set-ligatures '(js-mode ts-mode) '(\"=>\" \"...\" \"??\"))"
  (let ((normalized-modes (mac-ligature--normalize-modes modes)))
    ;; Remove any existing entry for these modes
    (setq mac-ligature-ligatures-for-modes
          (cl-remove-if (lambda (entry)
                          (equal (car entry) normalized-modes))
                        mac-ligature-ligatures-for-modes))
    ;; Add the new entry
    (push (cons normalized-modes ligatures) mac-ligature-ligatures-for-modes)
    ;; Refresh ligatures in all buffers if the mode is active
    (when (bound-and-true-p global-mac-ligature-mode)
      (mac-ligature--refresh-all-buffers))))

(defun mac-ligature-generate-ligatures ()
  "Generate composition rules for ligatures in the current buffer.
This function looks up which ligatures should apply to the current
major mode and sets up the buffer-local composition function table."
  (let ((ligatures (mac-ligature--get-ligatures-for-mode major-mode)))
    (when ligatures
      ;; Create a new char table that inherits from the global one
      (setq mac-ligature--composition-table
            (make-char-table 'composition-function-table))
      (set-char-table-parent mac-ligature--composition-table
                             composition-function-table)

      ;; Add composition rules for each character
      (dolist (char-pattern (mac-ligature--group-by-char ligatures))
        (let ((char (car char-pattern))
              (pattern (cdr char-pattern)))
          (set-char-table-range mac-ligature--composition-table char
                                `([,(concat "." pattern) 0 font-shape-gstring]))))

      ;; Set the buffer-local composition table
      (setq-local composition-function-table mac-ligature--composition-table)

      ;; Force redisplay
      (when (fboundp 'font-lock-flush)
        (font-lock-flush)))))

(defun mac-ligature--refresh-all-buffers ()
  "Refresh ligatures in all buffers where mac-ligature-mode is active."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when mac-ligature-mode
        (mac-ligature-generate-ligatures)))))

;;; ============================================================================
;;; Minor Mode
;;; ============================================================================

;;;###autoload
(define-minor-mode mac-ligature-mode
  "Toggle typographic ligatures in the current buffer.

When enabled, this mode uses the composition-function-table to
display configured ligatures based on the current major mode."
  :lighter " Lig"
  :group 'mac-ligature
  (if mac-ligature-mode
      (progn
        (mac-ligature-generate-ligatures)
        (add-hook 'after-change-major-mode-hook
                  #'mac-ligature-generate-ligatures nil t))
    ;; Disable: restore the global composition function table
    (kill-local-variable 'composition-function-table)
    (kill-local-variable 'mac-ligature--composition-table)
    (remove-hook 'after-change-major-mode-hook
                 #'mac-ligature-generate-ligatures t)
    (when (fboundp 'font-lock-flush)
      (font-lock-flush))))

(defun mac-ligature-mode-turn-on ()
  "Enable `mac-ligature-mode' if appropriate for the current buffer."
  (unless (or (minibufferp)
              (memq major-mode mac-ligature-ignored-major-modes))
    (mac-ligature-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-mac-ligature-mode
  mac-ligature-mode mac-ligature-mode-turn-on
  :group 'mac-ligature)

;;; ============================================================================
;;; Font Detection (Optional Enhancement)
;;; ============================================================================

(defun mac-ligature--font-supports-ligatures-p ()
  "Attempt to detect if the current font supports ligatures.
This is a best-effort heuristic and may not be 100% accurate."
  (let ((font (face-attribute 'default :family)))
    (or (string-match-p "JetBrains Mono" font)
        (string-match-p "Fira Code" font)
        (string-match-p "Cascadia Code" font)
        (string-match-p "Iosevka" font)
        (string-match-p "Hasklig" font)
        (string-match-p "Monoid" font)
        (string-match-p "Victor Mono" font))))

(provide 'mac-ligature)
;;; mac-ligature.el ends here
