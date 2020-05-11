;;; neut-mode.el --- neut mode -*- lexical-binding: t; -*-

;; Author: u2zv1wx <u2zv1wx@protonmail.ch>
;; Package-Requires: ((flycheck))

;;; Commentary:

;; A major mode for Neut.

;;; Code:

(require 'cl-lib)

(require 'flycheck)

(defvar neut-mode-syntax-table nil "Syntax table for `neut-mode'.")

;; cf. http://ergoemacs.org/emacs/elisp_syntax_coloring.html
(setq neut-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\; "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        (modify-syntax-entry ?? "w" synTable)
        (modify-syntax-entry ?. "w" synTable)
        (modify-syntax-entry ?@ "w" synTable)
        synTable))

;; https://stackoverflow.com/questions/9452615/emacs-is-there-a-clear-example-of-multi-line-font-locking
(defun test-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

(defconst neut-highlights
  `(("^-\n[[:ascii:][:nonascii:]]*?\n-$" 0 font-lock-comment-face keep t)
    (,(regexp-opt '("tau" "type" "universe") 'symbols) . font-lock-type-face)
    (,(regexp-opt '("theta") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("pi" "arrow" "Π" "forall" "hom") 'symbols) . font-lock-type-face)
    (,(regexp-opt '("pi-introduction" "pi-elimination" "lambda" "λ" "apply" "begin") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("sigma" "∑" "product" "exists") 'symbols) . font-lock-type-face)
    (,(regexp-opt '("sigma-introduction" "tuple" "construct" "sigma-elimination" "destruct") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("fix") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("enum" "enum-introduction" "enum-elimination" "switch" "default") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("array") 'symbols) . font-lock-type-face)
    (,(regexp-opt '("array-introduction" "array-elimination") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("struct") 'symbols) . font-lock-type-face)
    (,(regexp-opt '("struct-introduction" "struct-elimination") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("notation" "constant" "keyword" "let" "include" "statement") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("with") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("definition" "define" "ascription") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("splice") 'symbols) . font-lock-builtin-face)
    (,(regexp-opt '("admit" "?admit") 'symbols) . font-lock-warning-face)
    (,(regexp-opt '("unsafe") 'symbols) . font-lock-warning-face)
    (,(regexp-opt '("hole") 'symbols) . font-lock-warning-face)
    (,(regexp-opt '("zeta") 'symbols) . font-lock-warning-face)
    (,(regexp-opt '("inductive" "case") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("record") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("theorem" "proposition" "lemma" "corollary") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("assume") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("implicit") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("attribute") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("ensure") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("use" "unuse") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("introspect") 'symbols) . font-lock-builtin-face)
    (,(regexp-opt '("no-implicit-core") 'symbols) . font-lock-builtin-face)
    (,(regexp-opt '("section" "end") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("reify" "reflect") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("witness") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("verify") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("if") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("question") 'symbols) . font-lock-builtin-face)
    (,(regexp-opt '("erase") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("provide") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("irreducible") 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("*") 'symbols) . font-lock-warning-face)
    (,(regexp-opt '("_") 'symbols) . font-lock-warning-face)
    ))

(define-derived-mode neut-mode prog-mode "neut"
  "A major mode for Neut programming language."
  (setq font-lock-defaults '(neut-highlights))
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'font-lock-extend-region-functions
            'test-font-lock-extend-region)
  (set (make-local-variable 'comment-start) ";;")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table neut-mode-syntax-table)
  (flycheck-define-checker neut
    "A syntax checker fon Neut using the compiler."
    :command ("neut" "check" "--no-color" "--end-of-entry" "EOE" source-inplace) ;; inplace must be used to handle relative include
    :error-patterns
    ((info line-start (file-name) ":" line ":" column ":" (? "\r") "\n"
           "note: " (message (minimal-match (one-or-more anything))) (? "\r") "\n"
           "EOE" line-end)
     (warning line-start (file-name) ":" line ":" column ":" (? "\r") "\n"
              "warning: " (message (minimal-match (one-or-more anything))) (? "\r") "\n"
              "EOE" line-end)
     (error line-start (file-name) ":" line ":" column ":" (? "\r") "\n"
            "error: " (message (minimal-match (one-or-more anything))) (? "\r") "\n"
            "EOE" line-end)
     (error line-start (file-name) ":" line ":" column ":" (? "\r") "\n"
            "critical: " (message (minimal-match (one-or-more anything))) (? "\r") "\n"
            "EOE" line-end))
    :modes (neut-mode))
  (add-to-list 'flycheck-checkers 'neut))

(add-to-list 'auto-mode-alist '("\\.neut\\'" . neut-mode))

(setq-local comment-start "; ")
(setq-local comment-end "")

(provide 'neut-mode)

;;; neut-mode.el ends here
