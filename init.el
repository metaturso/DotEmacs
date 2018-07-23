;; -*- lexical-binding: t; -*-

(package-initialize)

(require 'cl-lib)

(defvar metaturso-minor-mode-map
  (let ((metaturso-map (make-sparse-keymap)))
    ;; Use buffer-menu instead of list-buffers since it opens in the same window.
    (define-key metaturso-map [remap list-buffers] 'buffer-menu)
    ;; Replace C-h C-f with a Emacs self-documenting function finder.
    (define-key metaturso-map [remap other-window] 'other-window-or-prompt)
    (define-key metaturso-map [remap move-beginning-of-line] 'move-beginning-of-line-or-text)
    (define-key metaturso-map [remap view-emacs-FAQ] 'find-function)
    metaturso-map)
  "One keymap to rule them all, or close to. This keymap should contain general Emacs
key bindings.")

;;;###autoload
(define-minor-mode metaturso-minor-mode
  "A global minor mode to centralise all of metaturso's Emacs customisations.

\\{metaturso-minor-mode-map}"
  :init-value t :global t :lighter nil)

;;;###autoload
(define-globalized-minor-mode global-metaturso-minor-mode metaturso-minor-mode t)

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

  ;; Enable all disabled commands.
  (setq disabled-command-function nil
	custom-file "~/.emacs-custom.el")

  ;; Disable audible or visible bell ESC or other events.
  (setq visible-bell 1)
  (setq ring-bell-function 'ignore)

  ;; Customise Emacs variables.
  (setq inhibit-startup-message t
	initial-scratch-message nil
	default-directory "~/"))

(defun metaturso-windows-after-init-hook nil
  "Windows configuration."
  (setq default-directory "~/Documents/Development/")
  ;; This should deter Emacs from using Windows line endings.
  (setq-default buffer-file-coding-system 'utf-8-unix))

;;;###autoload
(defun move-beginning-of-line-or-text nil
    "Toggles the cursor position between `beginning-of-line' and the first
non-whitespace character on the that line."
  (interactive)
  (let ((position (point)))
    (back-to-indentation)
    (when (equal position (point))
      (beginning-of-line))))

;;;###autoload
(defun other-window-or-prompt (count &optional all-frames)
  "Calls `other-window' as you'd expect from \\[C-x o] with the exception when
there is an active prompt. In this case the minibuffer
window will become active. If the prompt was active on a different frame than
the current one that frame will be gain focus."
  (interactive "p")
  (if (minibuffer-prompt)
      (unwind-protect
	  (let* ((minibuf (active-minibuffer-window))
		 (minibuf-frame (window-frame minibuf)))

	    (unless (equal minibuf-frame (selected-frame))
	      (select-frame minibuf-frame)
	      (raise-frame minibuf-frame))

	    (when (window-live-p minibuf)
	      (select-window minibuf))))
    (other-window count all-frames)))

;;; Initialisation Code
(let ((default-directory  "~/.emacs.d/")
      (lexical-binding nil))
  (normal-top-level-add-to-load-path '("lisp")))

;; Windows-specific initialisation hook.
(when (string= 'windows-nt system-type)
  (add-hook 'after-init-hook 'metaturso-windows-after-init-hook))

(add-hook 'after-init-hook 'metaturso-after-init-hook)
(add-hook 'before-save-hook 'metaturso-before-save-hook)
(add-hook 'wisent-grammar-mode-hook 'semantic-mode)
(add-hook 'prog-mode 'metaturso-keyword-highlighter)

;; For grammar development
(require 'metaturso-grammarian-minor-mode "grammarian-minor-mode.el")
(add-hook 'wisent-grammar-mode-hook 'metaturso-grammarian-minor-mode)
(add-hook 'json-mode-hook 'metaturso-grammarian-minor-mode)

(provide 'metaturso-minor-mode)
