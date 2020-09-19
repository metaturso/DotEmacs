;; -*- lexical-binding: t; -*-

(defvar metaturso-package-list nil
  "List of Emacs packages used by this configuration.
This list is used by `el-get' to sync packages.")

(push "cedet" metaturso-package-list)
(push "feature-mode" metaturso-package-list)

;;; Emacs Initialisation
;; Add directories to load-path.
(let ((dirs (list "lisp" "el-get/el-get/")))
  (defun add-to-load-path (dir)
    "Adds DIR directory to the `load-path'"
    (push (expand-file-name dir "~/.emacs.d") load-path))
  (mapc 'add-to-load-path dirs))

;; El-get bootstrap.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")
(el-get-cleanup metaturso-package-list)
(el-get 'sync metaturso-package-list)

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

(require 'cl-lib)
(require 'metaturso-minor-mode)

;; Operating-system specific changes to the configuration
(cond
 ((equal 'windows-nt system-type)
  (add-hook 'after-init-hook #'metaturso-windows-after-init-hook))

 ((equal 'darwin system-type)
  (add-hook 'after-init-hook #'metaturso-mac-after-init-hook)))

;; Load standalone CEDET to work with Semantic.
;; TODO: Replace this with an el-get's recipe for CEDET.
(when (and metaturso-ide-use-standalone-cedet metaturso-ide-standalone-cedet-directory)
  (load-file
   (expand-file-name "cedet-devel-load.el" metaturso-ide-standalone-cedet-directory)))

(global-ede-mode 1)

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
