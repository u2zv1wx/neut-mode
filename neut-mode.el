;;; neut-mode.el --- neut mode -*- lexical-binding: t; -*-

;; Author: u2zv1wx <u2zv1wx@protonmail.ch>

;;; Commentary:

;; A major mode for Neut.

;;; Code:

(define-derived-mode neut-mode prog-mode "neut"
  "A major mode for Neut programming language."
  (add-to-list 'auto-mode-alist '("\\.neut\\'" . neut-mode))
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?\; "<" syntax-table)
     (modify-syntax-entry ?\n ">" syntax-table)
     (modify-syntax-entry ?? "w" syntax-table)
     (modify-syntax-entry ?. "w" syntax-table)
     syntax-table))
  (setq font-lock-defaults
        `(,`((,(regexp-opt '("array" "arrow" "enum" "exists" "forall" "hom" "Pi" "product" "sigma" "struct" "tau" "type" "universe" "Π" "∑") 'symbols)
             . font-lock-type-face)
            (,(regexp-opt '("apply" "array-elimination" "array-introduction" "assume" "construct" "corollary" "default" "define" "destruct" "end" "ensure" "enum-elimination" "enum-introduction" "erase" "fix" "if" "include" "inductive" "irreducible" "lambda" "lemma" "let" "notation" "constant" "Π-elimination" "Π-introduction" "proposition" "record" "section" "sigma-elimination" "sigma-introduction" "statement" "struct-elimination" "struct-introduction" "switch" "theorem" "tuple" "unuse" "use" "with" "witness" "λ") 'symbols)
             . font-lock-keyword-face)
            (,(regexp-opt '("introspect" "question" "splice") 'symbols)
             . font-lock-builtin-face)
            (,(regexp-opt '("*" "?admit" "_" "admit") 'symbols)
             . font-lock-warning-face)
            ))))

(provide 'neut-mode)

;;; neut-mode.el ends here
