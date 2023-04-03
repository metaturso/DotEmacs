;;; Minimal Emacs startup file
;;
;; This file contains important settings that I want all Emacs
;; instances to use.

;; Disable version control integrations
(setq vc-handled-backends nil)

;; Disable audible or visible bell ESC or other events.
(setq visible-bell 1
      ring-bell-function 'ignore)

;; Default Find-File directory
(setq default-directory "~/")

;; Preload the prompts so that M-p can be used instead of typing.
(setq yes-or-no-p-history '("yes" "no"))

;;; Inhibit all startup messages.
;;
;; Disabling the echo area message needs atypical handling.  Read
;; https://yrh.dev/blog/rant-obfuscation-in-emacs/ for a good story.
;;
;; Replace RMS's echo area message function with one that does nothing
;; instead.
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Disable startup buffer and scratch message
(setq inhibit-startup-message t
      initial-scratch-message nil)

;;; Frame customisation
;;
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
