;; -*- lexical-binding: t; -*-

(defvar metaturso-default-directory
  (cond ((string= system-type "windows-nt") "~/Documents")
	((string-prefix-p "gnu" system-type) "~"))
  "The value of this variable is used to set `default-directory' to the least annoying
directory, based on the system wheren Emacs is running.")

(defvar metaturso-minor-mode-map
  (let ((metaturso-map (make-sparse-keymap)))
    (define-key metaturso-map [remap list-buffers] 'buffer-menu)
    ;; Replace C-h C-f with a Emacs self-documenting function finder.
    (define-key metaturso-map [remap view-emacs-FAQ] 'find-function)
    metaturso-map)
  "One keymap to rule them all, or close to. This keymap should contain general Emacs
key bindings.")

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
(define-minor-mode metaturso-minor-mode
  "A global minor mode to centralise all of metaturso's Emacs customisations.

Key Bindings:

\\{metaturso-minor-mode-map}"
  :init-value t :global t :lighter nil)

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
    (semantic-grammar-create-package t)
    (eval-buffer)
    (kill-buffer)
    (delete-window))
  )

;;;###autoload
(define-globalized-minor-mode metaturso-minor-mode metaturso-minor-mode t)

(defun metaturso-before-save-prog-hook ()
    "A hook to perform various programming cleanup routines before
the buffer is saved to its file."
    (whitespace-cleanup))

(defun metaturso-before-save-hook ()
  "A generic before-save hook used. Checks the current major-mode before calling
one or more functions to respond to the event."
  (when (or (derived-mode-p 'prog-mode)
	    (equal 'wisent-grammar-mode major-mode))
    (metaturso-before-save-prog-hook)))

(defun metaturso-after-init-hook ()
  "Misc configuration that needs to run as soon as Emacs starts."
  ;; Activate or disable global minor modes.
  (show-paren-mode)
  (blink-cursor-mode -1)

  ;; Record window actions and undo/redo with C-{left,right}.
  (winner-mode)
  ;; Move between adjacent windows with M-{left,up,right,down}
  (windmove-default-keybindings 'meta)

  ;; Add mode associations.
  (cl-pushnew '("\\.php\\'" . php-mode) auto-mode-alist)

  ;; Customise Emacs variables.
  (setq inhibit-startup-message t
	initial-scratch-message nil
	default-directory metaturso-default-directory))

(defun metaturso-keyword-highlighter nil
  "Customise the face of TODO, FIXME and NEXT to make them stand out."
    (font-lock-add-keywords nil '(("\\<\\(TODO\\|FIXME\\|NEXT\\):" 1 font-lock-warning-face t))))

(add-hook 'before-save-hook 'metaturso-before-save-hook)
(add-hook 'after-init-hook 'metaturso-after-init-hook)
(add-hook 'wisent-grammar-mode-hook 'semantic-mode)
(add-hook 'prog-mode 'metaturso-keyword-highlighter)

;; For grammar development
(add-hook 'wisent-grammar-mode-hook 'metaturso-grammarian-minor-mode)
(add-hook 'json-mode-hook 'metaturso-grammarian-minor-mode)
(add-hook 'php-mode-hook 'metaturso-grammarian-minor-mode)

(provide 'metaturso-minor-mode)
