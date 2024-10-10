(use-package emacs
  :hook ((after-init    . global-auto-revert-mode)
         (after-init    . recentf-mode)
         (after-init    . savehist-mode)
         (prog-mode     . auto-fill-mode))

  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo))

  :init
    (electric-pair-mode 1)
    (display-time-mode 1)
    (global-display-line-numbers-mode 1)
    (scroll-bar-mode 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)
    (blink-cursor-mode -1)
    (pixel-scroll-precision-mode)

    (setq history-length 25)
    (savehist-mode 1)
    (save-place-mode 1)
    (delete-selection-mode 1)
    (electric-indent-mode 1)
    (global-auto-revert-mode 1)

    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
    (setq undo-limit (* 64 1024 1024)) ; 64 mb
    (setq undo-strong-limit (* 96 1024 1024))
    (setq undo-outer-limit (* 90 1024 1024))

    (add-to-list 'default-frame-alist '(font . "Comic Shanns Mono-12"))
    (setq fill-column 80)
    (setq frame-resize-pixelwise t)
    (setq split-width-threshold 80)
    (setq create-lockfiles nil)
    (setq make-backup-files nil)
    (setq inhibit-startup-message t)
    (setq load-prefer-newer t)
    (setq select-enable-clipboard t)
    (setq confirm-nonexistent-file-or-buffer t)
    (setq fast-but-imprecise-scrolling t)
    (setq auto-save-default t)

    (setopt auto-revert-avoid-polling t)
    (setopt auto-revert-interval 5)
    (setopt initial-major-mode 'fundamental-mode)
    (setopt sentence-end-double-space nil)
    (setopt x-underline-at-descent-line nil)


    (setq display-line-numbers-width 3)
    (setq left-fringe-width 5)
    (setq right-fringe-width 5)
    (setq truncate-lines t)
    (setq indent-tabs-mode nil)
    (setq tab-width 2)

    (setq inhibit-startup-echo-area-message (user-login-name))
    (setq tab-always-indent 'complete)
    (setq read-extended-command-predicate #'command-completion-default-include-p))


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package jinx
   :ensure t
   :defer  t
   :hook (after-init . global-jinx-mode)
   :bind (("M-$" . jinx-correct)
          ("C-M-$" . jinx-languages)))


(use-package rainbow-delimiters
   :ensure t
   :defer  t
   :hook (prog-mode  . rainbow-delimiters-mode))


(use-package exec-path-from-shell
   :ensure t
   :defer  t)

(use-package project
   :ensure t
   :defer  t
   :config
   (setq project-vc-extra-root-markers '(".project.el" ".projectile" )))

(use-package eldoc-box
   :ensure t
   :defer  t
   :hook ((eglot-managed-mode . eldoc-box-hover-mode))
   :init (eldoc-box-hover-mode))

(use-package undo-fu
   :ensure t
   :defer  t)

(use-package elpher
   :ensure t
   :defer  t)

(use-package corfu
   :ensure t
   :defer  t
   :config
   (setq corfu-auto 1)
   (setq corfu-preselect 'prompt)
   (setq corfu-cycle t)
   :init (global-corfu-mode))

(use-package kind-icon
   :ensure t
   :defer  t
   :after corfu
   :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
   :ensure t
   :custom
   (completion-styles '(orderless flex))
   (completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
   :ensure t
   :defer  t
   :init (marginalia-mode))

(use-package moody
   :ensure t
   :config
   (moody-replace-mode-line-front-space)
   (moody-replace-mode-line-buffer-identification)
   (moody-replace-vc-mode))

(use-package beacon
   :ensure t
   :defer  t
   :init (beacon-mode 1))

(use-package vertico
    :ensure t
    :defer  t
    :init (vertico-mode))

(use-package consult
   :ensure t
   :defer  t
   :hook (completion-list-mode . consult-preview-at-point-mode)
   :bind (("C-x b"    .  consult-buffer)
          ("M-g M-g"  .  consult-goto-line)
	  ("M-s d"    .  consult-fd)
	  ("M-s r"    .  consult-ripgrep)
          ("C-y"      .  consult-yank-from-kill-ring)
          ("C-s"      .  consult-line)))

(use-package auto-virtualenv
   :ensure t
   :defer  t
   :hook (python-ts-mode  . auto-virtualenv-set-virtualenv))

(use-package haskell-mode
   :ensure t
   :defer  t)

(use-package sly
   :ensure t
   :defer  t)

(use-package eglot
   :ensure t
   :defer  t
   :hook ((haskell-mode    . eglot-ensure)
          (c-ts-mode       . eglot-ensure)
          (python-ts-mode  . eglot-ensure))
   :init
     (setq eglot-sync-connect 1
           eglot-connect-timeout 5
           eglot-autoshutdown t
           eglot-send-changes-idle-time 45
           jsonrpc-event-hook nil)

     (fset #'jsonrpc--log-event #'ignore)

     (setq completion-category-overrides '((eglot (styles orderless))
                                           (eglot-capf (styles orderless)))

           eglot-workspace-configuration '((haskell (plugin
                                              (stan (globalOn . :json-false))
                                              (splice (globalOn . :json-false))
                                              (eval (globalOn . :json-false)))
                                              (formattingProvider "fourmolu")))))

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters moody catppuccin-theme sly haskell-mode pyenv auto-virtualenv)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
