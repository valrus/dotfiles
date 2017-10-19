;;; private/hlissner/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "imccowan@gmail.com"
      user-full-name    "Ian McCowan")

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq doom-theme 'doom-one-light)
(global-vi-tilde-fringe-mode -1)

;; fonts
(set!
  :font "Iosevka"
  :size 11
  :weight 'light)
(set! :big-font "Iosevka Slab" :size 16)
(set! :variable-font "Fira Sans" :size 11)
(set! :unicode-font "DejaVu Sans Mono" :size 11)
(setq +doom-modeline-height 20)

(dolist (name-and-attrs (directory-files-and-attributes "~/Code" 'full))
  (let ((name (car name-and-attrs))
        (attrs (cdr name-and-attrs)))
    (when (eq t (nth 0 attrs))
      (projectile-add-known-project name))))
(projectile-add-known-project "~/.emacs.d")
