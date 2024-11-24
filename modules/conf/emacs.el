;;; package --- init.el
;;; Commentary:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package emacs
  :hook ((after-init    . global-auto-revert-mode)
         (after-init    . recentf-mode)
         (after-init    . savehist-mode)
       	 (prog-mode     . display-line-numbers-mode))

  :init

    (defun set-font-faces ()
      "Set fonts"
      (defvar mono-font "Rec Mono Casual-12")
      (defvar sans-font "Recursive Sans Casual Static-12")
      (set-face-attribute 'default nil
                          :height 12
                  	  :font mono-font
			  :weight 'medium)
      
      (set-face-attribute 'mode-line nil
                  	  :font mono-font
                          :weight 'bold)
      
      (set-face-attribute 'mode-line-active nil
                       	  :font mono-font
                          :weight 'bold)
      
      (set-face-attribute 'mode-line-inactive nil
                  	  :font mono-font)
      
      (set-face-attribute 'variable-pitch nil
                  	  :font sans-font))

    (set-font-faces)
    
    (if (daemonp)
        (add-hook 'after-make-frame-functions (lambda (frame) (with-selected-frame frame (set-font-faces)))))

    (electric-pair-mode 1)
    (display-time-mode 1)
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

    (setq custom-file (locate-user-emacs-file "custom-vars.el"))
    (load custom-file 'noerror 'nomessage)
    

    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
    (setq undo-limit (* 64 1024 1024)) ; 64 mb
    (setq read-process-output-max (* 1024 1024 4))
    
    (setq undo-strong-limit (* 96 1024 1024))
    (setq undo-outer-limit (* 90 1024 1024))

    (setq fill-column 80)
    (setq frame-resize-pixelwise t)
    (setq split-width-threshold 80)
    (setq create-lockfiles nil)
    (setq initial-scratch-message "")
    
    (setq make-backup-files nil)
    (setq auto-save-list-file-prefix "~/.config/emacs/autosave/")
    (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/autosave" t)))
    (setq inhibit-startup-message t)
    (setq load-prefer-newer t)
    (setq select-enable-clipboard t)
    (setq confirm-nonexistent-file-or-buffer t)
    (setq fast-but-imprecise-scrolling t)
    (setq auto-save-default t)
    (setq use-short-answers t)
    (setq warning-minimum-level :emergency)

    
    (setopt auto-revert-avoid-polling t)
    (setopt auto-revert-interval 5)
    (setopt initial-major-mode 'fundamental-mode)
    (setopt sentence-end-double-space nil)
    (setopt x-underline-at-descent-line nil)
    (setopt pgtk-wait-for-event-timeout 0)

    (setq display-line-numbers-width 3)
    (setq left-fringe-width 5)
    (setq right-fringe-width 5)
    (setq truncate-lines t)
    (setq indent-tabs-mode nil)
    (setq tab-width 2)

    (setq treesit-font-lock-level 4)
    (setq inhibit-startup-echo-area-message (user-login-name))
    (setq tab-always-indent 'complete)
    (setq read-extended-command-predicate #'command-completion-default-include-p))

    

    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package jinx
   :ensure t
   :defer  t
   :hook (after-init . global-jinx-mode)
   :bind (("M-$" . jinx-correct)
          ("C-M-$" . jinx-languages)))


(use-package flymake-collection
  :ensure t
  :defer  t
  :hook ((after-init . flymake-collection-hook-setup)
         (emacs-lisp-mode . flymake-mode)))

(use-package treesit-auto
  :ensure  t
  :defer   4
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package rainbow-delimiters
   :ensure t
   :hook (prog-mode  . rainbow-delimiters-mode))


(use-package exec-path-from-shell
   :ensure t
   :defer  2
   :init (exec-path-from-shell-initialize))

(use-package project
   :defer  t
   :config
   (setq project-vc-extra-root-markers '(".project.el" ".projectile" )))

(use-package undo-fu
   :ensure t
   :bind ("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fy-only-redo))

(use-package elpher
   :ensure t
   :commands (elpher))

(use-package corfu
   :ensure t
   :defer  2
   :custom
   (corfu-auto t)
   (corfu-preselect 'first)
   (corfu-cycle t)
   (corfu-popupinfo-delay '(0.8 . 0.8))
   (corfu-quit-no-match 'separator)
   :config
   (keymap-unset corfu-map "RET")
   :init
   (global-corfu-mode 1)
   (corfu-popupinfo-mode 1))

(use-package kind-icon
   :ensure t
   :after corfu
   :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
   :ensure t
   :custom
   (completion-styles '(orderless flex))
   (completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
   :ensure t
   :defer  2
   :init (marginalia-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-project-detection 'auto)
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(use-package nerd-icons
  :ensure t
  :defer  2)

(use-package modus-themes
  :ensure t
  :defer  t
  :init (load-theme 'modus-operandi-tritanopia :no-confirm))

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
	  ("M-g f"    .  consult-flymake)
          ("M-g e"    .  consult-compile-error)
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
   :defer  t
   :custom (inferior-lisp-program "sbcl"))

(use-package geiser-guile
  :ensure  t)

(use-package puni
  :ensure  t
  :defer   2
  :init
  (puni-global-mode))

(use-package eglot
   :defer  t
   :hook ((haskell-mode    . eglot-ensure)
          (c-ts-mode       . eglot-ensure)
          (python-ts-mode  . eglot-ensure))

   :init
     (setq eglot-sync-connect 1
           eglot-connect-timeout 5
           eglot-autoshutdown t
           eglot-send-changes-idle-time 45)

     (setq completion-category-overrides '((eglot (styles orderless))
                                           (eglot-capf (styles orderless)))

           eglot-workspace-configuration '((haskell (plugin
                                              (stan (globalOn . :json-false))
                                              (splice (globalOn . :json-false))
                                              (eval (globalOn . :json-false)))
                                              (formattingProvider "fourmolu")))))
(provide 'init)
;;; init.el ends here
