;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(load (expand-file-name "startup-minimal" user-emacs-directory) nil t nil)

;;; Emacs Initialisation
;; Add directories to load-path.
(let ((dirs (list "lisp" "el-get/el-get/")))
  ;; Remove builtin CEDET from the load-path.
  ;; TODO: Move this inside el-get configuration code.
  (setq load-path (cl-remove-if (lambda (path) (string-match-p "cedet" path)) load-path))

  ;; Add custom lisp to the load-path.
  (defun add-to-load-path (dir)`
    "Adds DIR directory to the `load-path'"
    (push (expand-file-name dir user-emacs-directory) load-path))
  (mapc 'add-to-load-path dirs))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(cl-pushnew (expand-file-name "recipes" user-emacs-directory) el-get-recipe-path)

;;; Package configuration
(el-get-bundle yaml-mode)
(el-get-bundle feature-mode)
(el-get-bundle highlight-parentheses)

(unless (equal 'windows-nt system-type)
  (el-get-bundle cedet (global-ede-mode 1)))

;; Theme
(load-theme 'wombat t)

;; Activate or disable global minor modes.
(show-paren-mode)

;; Record window actions and undo/redo with C-{left,right}.
(winner-mode)
;; Move between adjacent windows with M-{left,up,right,down}
(windmove-default-keybindings 'meta)

;; Add auto-mode file associations.
(cl-pushnew '("Cask\\'" . emacs-lisp-mode) auto-mode-alist)

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Write changes made by customize to a separate file.
(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))

(require 'metaturso-minor-mode)

;; Operating-system specific changes to the configuration
(cond
 ((equal 'windows-nt system-type)
  (add-hook 'after-init-hook #'metaturso-windows-after-init-hook))

 ((equal 'darwin system-type)
  (add-hook 'after-init-hook #'metaturso-mac-after-init-hook)))

(add-hook 'wisent-grammar-mode-hook #'semantic-mode)

;; For grammar development
;; Breaks on Emacs for Mac (triggers the nasty grammar compilation issue)
;; still unsure as to what's causing this.
;;(require 'metaturso-grammarian-minor-mode "grammarian-minor-mode.el")
;;(add-hook 'wisent-grammar-mode-hook 'metaturso-grammarian-minor-mode)
;;(add-hook 'composer-file-mode-hook 'metaturso-grammarian-minor-mode)

;;; Hooks
(defun metaturso-mac-after-init-hook nil
  "Mac configuration hook."
  (setq-default mac-right-option-modifier nil))

(defun metaturso-windows-after-init-hook nil
  "Windows configuration hook."
  (setq default-directory "~/Documents/")
  ;; This should deter Emacs from using Windows line endings.
  (setq-default buffer-file-coding-system 'utf-8-unix))
