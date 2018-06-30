;; -*- lexical-binding: t; -*-

(require 'semantic/grammar)

(defvar metaturso-grammarian-minor-mode-map
  (let ((grammarian-map (make-sparse-keymap)))
    (define-key grammarian-map (kbd "C-c t") 'semantic-lex-test)
    (define-key grammarian-map (kbd "<f5>") 'metaturso-grammarian-bovinate-reparse-buffer)
    (define-key grammarian-map (kbd "S-<f9>") 'metaturso-grammarian-compile-grammar)
    (define-key grammarian-map (kbd "C-c l") 'semantic-lex-debug)
    (define-key grammarian-map (kbd "C-c a") 'semantic-analyze-current-context)
    grammarian-map)
  "The grammarian minor mode keymap binds helpful lexer and grammar functions.")

;;;###autoload
(define-minor-mode metaturso-grammarian-minor-mode
  "A minor mode to streamline and simplify my Wisent grammar development workflow

\\{metaturso-grammarian-minor-mode-map}"
  :init-value nil :lighter " Grammarian" :keymap metaturso-grammarian-minor-mode-map
  (semantic-mode)
  (setq wisent-verbose-flag t))

(defun metaturso-grammarian-bovinate-reparse-buffer nil
  "Bovinate the entire buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (bovinate -1)))

(defun metaturso-grammarian-compile-grammar nil
  "Compiles and reloads the grammar by evaluating the resulting lisp code."
  (interactive)
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (semantic-grammar-create-package t)
      (eval-buffer)
      (kill-buffer)
      (delete-window)
      (switch-to-buffer old-buffer))))

(provide 'metaturso-grammarian-minor-mode)
