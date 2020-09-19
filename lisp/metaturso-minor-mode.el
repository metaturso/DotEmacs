(defgroup metaturso-ide nil
  "Emacs IDE configuration."
  :prefix "metaturso-ide-"
  :group 'metaturso)

(defcustom metaturso-ide-use-standalone-cedet
  t
  "When non-nil the standalone CEDET package instead of using Emacs built-in.
When this is non-nil you must also set `metaturso-cedet-standalone-directory'."
  :type 'boolean
  :group 'metaturso-ide)

(defcustom metaturso-ide-standalone-cedet-directory
  nil
  "Path to the standalone CEDET package."
  :type 'string
  :group 'metaturso-ide)

(defconst metaturso-minor-mode-map
  (let ((metaturso-map (make-sparse-keymap)))
    ;; Use buffer-menu instead of list-buffers since it opens in the same window.
    (define-key metaturso-map [remap list-buffers] 'buffer-menu)
    ;; Replace C-h C-f with a Emacs self-documenting function finder.
    (define-key metaturso-map [remap other-window] 'other-window-or-prompt)
    (define-key metaturso-map [remap move-beginning-of-line] 'move-beginning-of-line-or-text)
    (define-key metaturso-map [remap view-emacs-FAQ] 'find-function)
    metaturso-map)
  "One keymap to rule them all (or close to). This keymap contains personal Emacs
key bindings which are useful across a variety of major modes.")

;;;###autoload
(define-minor-mode metaturso-minor-mode
  "A global minor mode to centralise all of metaturso's Emacs customisations.

\\{metaturso-minor-mode-map}"
  :init-value t :global t :lighter nil
  :group 'metaturso)

;;;###autoload
(define-globalized-minor-mode global-metaturso-minor-mode metaturso-minor-mode
  turn-on-metaturso-minor-mode
  :group 'metaturso)

(defun turn-on-metaturso-minor-mode ()
  "Turns on `metaturso-minor-mode'"
  (metaturso-minor-mode 1))

(add-hook 'before-save-hook #'metaturso-before-save-hook)

;; Hooks
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

;;; Editing support functions.
(defun move-beginning-of-line-or-text nil
  "Toggles the cursor position between `beginning-of-line' and the first
non-whitespace character on the that line. Always jumps to the beginning
of the line first, press again to jump to the indentation point."
  (interactive)
  (let ((position (point)))
    (beginning-of-line)
    (when (equal position (point))
      (back-to-indentation))))

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

(provide 'metaturso-minor-mode)
