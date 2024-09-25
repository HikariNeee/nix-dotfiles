(require 'package)
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; PACKAGES
(use-package rainbow-delimiters
  :ensure t)

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
  :defer t
  :ensure t
  :init (eldoc-box-hover-mode))

(use-package undo-fu
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

(use-package sly
  :defer t
  :ensure t)

(use-package haskell-ts-mode
  :defer t
  :ensure t)


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
  :hook
  (after-init . vertico-mode))

(use-package nix-ts-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(use-package consult
  :ensure t
  :demand t
  :bind (
	 ("C-c M-x"  .  consult-mode-command)
	 ("C-x b"    .  consult-buffer)
	 ("M-g g"    .  consult-goto-line)
	 ("M-g M-g"  .  consult-goto-line)
	 ("M-s d"    .  consult-fd)
	 ("M-s r"    .  consult-ripgrep)
         ("C-y"      .  consult-yank-from-kill-ring)
         ("C-s"      .  consult-line)
         :map minibuffer-local-map
 	 ("M-s" .     consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init)


(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))


(use-package nerd-icons
  :ensure t)


(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package exec-path-from-shell
  :ensure t)


(use-package auto-virtualenv ;; For Python
  :ensure t
  :defer t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))


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

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package yasnippet
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package consult-hoogle
  :defer t
  :ensure t)

;; EGLOT
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(c-ts-mode . ("clangd15" "--stdio")))
  (haskell-ts-setup-eglot))

(setq eglot-sync-connect 1
      eglot-connect-timeout 5
      eglot-autoshutdown t
      eglot-send-changes-idle-time 45
      eglot-auto-display-help-buffer nil
      eglot-events-buffer-size 0)

(fset #'jsonrpc--log-event #'ignore)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
(add-hook 'haskell-ts-mode 'prettify-symbols-mode)


;; LSP
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))



(setq eglot-workspace-configuration '((haskell (plugin
                                                (stan (globalOn . :json-false))
                                                (splice (globalOn . :json-false))
                                                (eval (globalOn . :json-false)))
                                               (formattingProvider "fourmolu"))))

;; EMACS improvements

(setq native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 1.0
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa 1
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      inhibit-compacting-font-caches t
      custom-safe-themes t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; CHARSETS

(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)


(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; UI

(setq display-time-24hr-format t
      truncate-lines t
      fill-column 80
      line-move-visual t
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      split-width-threshold 80
      create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
      display-time-default-load-average nil
      inhibit-startup-message t
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
      uniquify-buffer-name-style 'forward
      mouse-yank-at-point t
      backup-by-copying-when-linked t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 5
      vc-make-backup-files nil
      resize-mini-windows 'grow-only
      fast-but-imprecise-scrolling t
      auto-save-default t
      hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      blink-matching-paren nil
      x-stretch-cursor nil
      truncate-partial-width-windows nil)

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

(blink-cursor-mode 0)
(display-battery-mode)
(electric-pair-mode)
(display-time-mode)
(global-display-line-numbers-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq history-length 25)     ;; History Length
(savehist-mode 1)            ;; Save history
(save-place-mode 1)          ;; Save place in files
(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode 1)     ;; Indents
(global-auto-revert-mode 1)  ;; Automatically show changes if the file has changed
(prettify-symbols-mode 1)    ;; Combine symbols


(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; KEYBINDS
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; ORG
(setq org-agenda-files '("~/org")
      org-log-done 'time
      org-return-follows-link  t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


;; FONT
(add-to-list 'default-frame-alist '(font . "Comic Shanns Mono-12"))

;; THEME
(use-package catppuccin-theme :ensure t)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

