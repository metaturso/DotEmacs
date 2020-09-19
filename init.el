;; -*- lexical-binding: t; -*-

;;; Emacs Initialisation
;; Add the ~/.emacs.d/lisp directory to load-path.
(let ((default-directory  "~/.emacs.d/")
      (lexical-binding nil))
  (normal-top-level-add-to-load-path '("lisp")))

;;; El-Get integration
(add-to-list 'load-path (expand-file-name "el-get/" default-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (expand-file-name "el-get-user/recipes" default-directory))
(el-get 'sync)

(require 'cl-lib)
(require 'metaturso-minor-mode)

;; Operating-system specific changes to the configuration
(cond
 ((equal 'windows-nt system-type)
  (add-hook 'after-init-hook #'metaturso-windows-after-init-hook))

 ((equal 'darwin system-type)
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (add-hook 'after-init-hook #'metaturso-mac-after-init-hook)))

(package-initialize)
(when (featurep 'cask)
  (cask-initialize))

;; Theme
(load-theme 'wombat t)

;; Activate or disable global minor modes.
(show-paren-mode)
(blink-cursor-mode -1)
(tool-bar-mode -1)

;; Record window actions and undo/redo with C-{left,right}.
(winner-mode)
;; Move between adjacent windows with M-{left,up,right,down}
(windmove-default-keybindings 'meta)

;; Add auto-mode file associations.
(cl-pushnew '("\\.php\\'" . php-mode) auto-mode-alist)
(cl-pushnew '("Cask\\'" . emacs-lisp-mode) auto-mode-alist)
(cl-pushnew '("composer.json\\'" . composer-file-mode) auto-mode-alist)
(cl-pushnew '("composer.lock\\'" . composer-file-mode) auto-mode-alist)

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Write changes made by customize to a separate file.
(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Disable audible or visible bell ESC or other events.
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; Customise Emacs variables.
(setq inhibit-startup-message t
      initial-scratch-message nil
      default-directory "~/")

;; Preload the prompts so that M-p can be used instead of typing.
(setq yes-or-no-p-history '("yes" "no"))

;; Load standalone CEDET to work with Semantic.
(when (and metaturso-ide-use-standalone-cedet metaturso-ide-standalone-cedet-directory)
  (load-file
   (expand-file-name "cedet-devel-load.el" metaturso-ide-standalone-cedet-directory)))

(global-ede-mode 1)

(add-hook 'before-save-hook #'metaturso-before-save-hook)
(add-hook 'wisent-grammar-mode-hook #'semantic-mode)
(add-hook 'prog-mode #'metaturso-keyword-highlighter)

;; For grammar development
;; Breaks on Emacs for Mac (triggers the nasty grammar compilation issue)
;; still unsure as to what's causing this.
;;(require 'metaturso-grammarian-minor-mode "grammarian-minor-mode.el")
;;(add-hook 'wisent-grammar-mode-hook 'metaturso-grammarian-minor-mode)
;;(add-hook 'composer-file-mode-hook 'metaturso-grammarian-minor-mode)

;;; Hooks
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

(defun metaturso-windows-after-init-hook nil
  "Windows configuration hook."
  (setq default-directory "~/Documents/Development/")
  ;; This should deter Emacs from using Windows line endings.
  (setq-default buffer-file-coding-system 'utf-8-unix))

(defun metaturso-mac-after-init-hook nil
  "Mac configuration hook."
  (setq-default mac-right-option-modifier nil))

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
