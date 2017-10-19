;;; private/valrus/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +valrus-dir (file-name-directory load-file-name))
(defvar +valrus-snippets-dir (expand-file-name "snippets/" +valrus-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +valrus-dir)))

(defun +valrus*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+valrus*no-authinfo-for-tramp)

;;
(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))


;;
;(after! doom-themes
;  (load-theme doom-one-light t))

(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+valrus-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))


(def-package! pyenv-mode
  :demand t
  :after python
  :init
  (add-hook 'python-mode-hook #'pyenv-mode))

(def-package! kivy-mode
  :after python
  :mode ("*.kv" . kivy-mode))
