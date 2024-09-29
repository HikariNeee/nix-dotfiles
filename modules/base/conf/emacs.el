(require 'package)
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package emacs
  :hook ((prog-mode . rainbow-delimiters)
         (after-init . global-auto-revert-mode)
         (after-init . recentf-mode)
         (after-init . savehist-mode))

  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo))
  :init
  (setq native-comp-async-report-warnings-errors 'silent
        byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
        bidi-display-reordering 'left-to-right
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa 1
        cursor-in-non-selected-windows nil
        highlight-nonselected-windows nil
        inhibit-compacting-font-caches t
        custom-safe-themes t)

  (setq truncate-lines t
        fill-column 80
        line-move-visual t
        frame-resize-pixelwise t
        window-resize-pixelwise nil
        split-width-threshold 80)

  (setq create-lockfiles nil
        make-backup-files nil
        version-control t
        backup-by-copying t
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        backup-by-copying-when-linked t
        backup-by-copying t
        delete-old-versions t
        vc-make-backup-files nil
        backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))

  (setq inhibit-startup-message t
        confirm-kill-processes nil
        load-prefer-newer t
        x-select-enable-clipboard t
        switch-to-buffer-obey-display-actions t
        show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        whitespace-line-column nil
        rainbow-delimiters-max-face-count 5
        confirm-nonexitent-file-or-buffer nil
        uniquify-buffer-name-style 'forward)

  (setq mouse-yank-at-point t
        resize-mini-windows 'grow-only
        fast-but-imprecise-scrolling t
        hscroll-margin 2
        hscroll-step 1
        scroll-conservatively 10
        scroll-margin 0
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        x-stretch-cursor nil
        truncate-partial-width-windows nil
        auto-save-default t
        auto-save-include-big-deletions t)

  (setq-default display-line-numbers-width 3
                display-line-numbers-widen t
                left-fringe-width 8
                right-fringe-width 8
                indicate-buffer-boundaries nil
                indicate-empty-lines nil
                word-wrap t
                truncate-lines t
                indent-tabs-mode nil
                electric-indent-inhibit t
                inhibit-startup-echo-area-message (user-login-name)
                display-line-numbers-width 3
                inhibit-major-mode 'fundamental-mode)

  (electric-pair-mode 1)
  (display-time-mode 1)
  (global-display-line-numbers-mode 1)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  (setq history-length 25)
  (savehist-mode 1)
  (save-place-mode 1)
  (delete-selection-mode 1)
  (electric-indent-mode 1)
  (global-auto-revert-mode 1)

  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 100663296) ; 90mb.

  (add-to-list 'default-frame-alist '(font . "Comic Shanns Mono-12")))

(use-package org
  :hook ((org-mode  . org-indent-mode)
         (org-mode  . visual-line-mode))

  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(use-package eglot
 :defer t
 :ensure t
 :hook ((haskell-mode    . eglot-ensure)
        (clojure-ts-mode . eglot-ensure)
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

;; PACKAGES
;; BASICS
(use-package eglot-booster
        :after eglot
        :config (eglot-booster-mode))

(use-package rainbow-delimiters
  :defer t
  :ensure t)

(use-package projectile
  :defer t
  :ensure t
  :init (projectile-mode)
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '(("~/opt" . 1))))

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :defer t
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "◉ ")
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))


(use-package eldoc-box
  :ensure t
  :defer  t
  :hook ((eglot-managed-mode . eldoc-box-hover-mode))
  :init (eldoc-box-hover-mode))

(use-package elpher
  :ensure t
  :defer t)

(use-package undo-fu
  :defer t
  :ensure t)

(use-package corfu
  :ensure t
  :defer t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-auto t)
  (tab-always-indent 'complete)

  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	("S-TAB" . corfu-previous)))

(use-package orderless
  :ensure t
  :demand t
  :init
   (setq completion-styles '(orderless partial-completion basic)
         completion-category-defaults nil
         completion-category-overrides nil))

(use-package gcmh
  :ensure t
  :demand t
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 64 1024 1024))) ; 64mb


(use-package beacon
  :ensure t
  :defer t
  :config
  (beacon-mode))


(use-package vertico
  :ensure t
  :defer t
  :hook
  (after-init . vertico-mode))

(use-package consult
  :ensure t
  :defer t
  :bind (
	 ("C-c M-x"  .  consult-mode-command)
	 ("C-x b"    .  consult-buffer)
	 ("M-g M-g"  .  consult-goto-line)
	 ("M-s d"    .  consult-fd)
	 ("M-s r"    .  consult-ripgrep)
         ("C-y"      .  consult-yank-from-kill-ring)
         ("C-s"      .  consult-line)
         :map minibuffer-local-map
 	 ("M-s"      .     consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode))


(use-package marginalia
  :ensure t
  :defer t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))


(use-package nerd-icons
  :defer t
  :ensure t)


(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package exec-path-from-shell
  :defer t
  :ensure t)


(use-package auto-virtualenv ;; For Python
  :ensure t
  :defer t
  :init
  (use-package pyvenv
    :ensure t)
  :hook (python-ts-mode . auto-virtualenv-set-virtualenv))


(use-package indent-guide
  :ensure t
  :defer t
  :config
  (indent-guide-global-mode)
  (setq indent-guide-char ":")
  (setq indent-guide-delay 0.1)
  (set-face-background 'indent-guide-face "dimgray"))


(use-package jinx
  :ensure t
  :demand t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package yasnippet
  :defer t
  :ensure t)

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;; LANGUAGES
(use-package nix-ts-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(use-package sly
  :defer t
  :ensure t)

(use-package haskell-mode
  :defer t
  :ensure t)

(use-package clojure-ts-mode
  :defer t
  :ensure t)
