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

(provide 'metaturso-minor-mode)
