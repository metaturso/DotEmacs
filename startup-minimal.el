;;; Minimal Emacs startup file
;;
;; This file contains important settings that I want all Emacs
;; instances to use.

;; Disable version control integrations
(setq vc-handled-backends nil)

;; Disable audible or visible bell ESC or other events.
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; Customise Emacs variables.
(setq inhibit-startup-message t
      initial-scratch-message nil
      default-directory "~/")

;; Preload the prompts so that M-p can be used instead of typing.
(setq yes-or-no-p-history '("yes" "no"))

;; Customise frame
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
