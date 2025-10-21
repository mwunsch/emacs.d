;;; init.el --- My Emacs configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Personal Emacs configuration following technomancy's better-defaults philosophy
;; with modern tooling for project management, syntax checking, and LSP support.

;;; Code:

;;; ============================================================================
;;; Package Management
;;; ============================================================================

(require 'package)

;; Configure package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ============================================================================
;;; Editor Defaults
;;; ============================================================================
;; Based on better-defaults with some personal preferences

(use-package better-defaults)

;; Text editing behavior
(delete-selection-mode 1)              ; Replace selection when typing
(setq create-lockfiles nil)            ; Don't create .# lockfiles
(column-number-mode 1)                 ; Show column number in mode line
(setq use-short-answers t)             ; Use "y" or "n" instead of "yes" or "no

;; Automatically reload files when changed on disk
(global-auto-revert-mode 1)            ; Auto-revert buffers when files change
(setq auto-revert-avoid-polling t)     ; Use file notifications instead of polling

;; Programming mode enhancements
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Tab bar for managing window configurations as tabs
;; Behaves like native macOS tabs
(use-package tab-bar
  :ensure nil
  :config
  (tab-bar-mode 1)
  (setq tab-bar-show 1)                      ; Show tab bar only when > 1 tab
  (setq tab-bar-new-tab-choice "*scratch*")  ; What to show in new tabs
  (setq tab-bar-new-tab-to 'rightmost)       ; Place new tabs on the right
  (setq tab-bar-close-button-show nil)       ; Hide close button (cleaner)
  (setq tab-bar-tab-hints t)                 ; Show tab numbers for quick switching
  :bind (("s-t" . tab-new)                   ; Cmd+T for new tab
         ("s-w" . tab-close)                 ; Cmd+W to close tab
         ("s-{" . tab-previous)              ; Cmd+{ for previous tab
         ("s-}" . tab-next)))                ; Cmd+} for next tab

;; Discover keybindings as you type
(use-package which-key
  :config (which-key-mode))

;;; ============================================================================
;;; Minibuffer Completion (Ido)
;;; ============================================================================
;; Ido provides enhanced minibuffer completion for files, buffers, etc.

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)

;; Enhanced Ido completion for M-x and other commands
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode 1))

;; Vertical display for Ido choices
(use-package ido-vertical-mode
  :config (ido-vertical-mode 1)
  :custom (ido-vertical-define-keys 'C-n-C-p-up-and-down))

;;; ============================================================================
;;; Appearance & Theme
;;; ============================================================================

;; Font configuration (SF Mono if available)
(when (member "SF Mono" (font-family-list))
  (set-face-attribute 'default nil :family "SF Mono" :height 124))

;; Doom theme for modern aesthetics
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Distinguish "real" buffers from special/temporary ones
(use-package solaire-mode
  :config (solaire-global-mode 1))

;; Modern modeline with cleaner information display
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-icon nil))

;; Show search match count in mode line
(use-package anzu
  :config (global-anzu-mode 1))

;;; ============================================================================
;;; Project & Version Control
;;; ============================================================================

;; Project management with built-in project.el
;; Key bindings: C-x p (default), C-c p (custom binding for muscle memory)
(use-package project
  :ensure nil
  :config
  (global-set-key (kbd "C-c p") project-prefix-map)
  (global-set-key (kbd "s-p") 'project-find-file))

;; Inherit PATH and environment from shell (important on macOS)
;; Configured to read .zshenv for Volta (node version manager)
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-shell-name "/bin/zsh")
  ;; Only load login shell config (.zshenv, .zprofile), skip .zshrc
  (exec-path-from-shell-arguments '("-l"))
  ;; Only copy essential variables (faster than copying everything)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (exec-path-from-shell-initialize))

;; Automatically load direnv environments
;; Essential for nix/devenv projects
(use-package direnv
  :config
  (direnv-mode))

;; Git interface
;; Key binding: M-x magit-status
(use-package magit
  :commands magit-status)

;; GitHub gist integration
(use-package gist
  :commands (gist-list gist-region-or-buffer))

;;; ============================================================================
;;; Development Tools
;;; ============================================================================

;; LSP (Language Server Protocol) support via built-in eglot
;; Provides IDE-like features: go-to-definition, find-references, etc.
;; Uses standard Emacs bindings (M-., M-?, etc.)
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :config
  ;; Disable event logging for better performance (use nil to disable)
  (setq eglot-events-buffer-config '(:size 0))
  ;; Shutdown server when last buffer is killed
  (setq eglot-autoshutdown t))

;; On-the-fly syntax checking via built-in flymake
;; Automatically enabled with eglot, displays LSP diagnostics
;; Key bindings: M-g n/p (next/prev error), C-h . (show error)
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (("M-g n" . flymake-goto-next-error)
         ("M-g p" . flymake-goto-prev-error)))

;; In-buffer completion
;; Navigate with M-n/M-p, complete with RET
(use-package company
  :hook (prog-mode . company-mode))

;; File tree sidebar
;; Key binding: C-c t
(use-package treemacs
  :bind ("C-c t" . treemacs)
  :custom
  (treemacs-width 30)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :config
  (setq treemacs-no-png-images t))

;; Treemacs integration with magit
(use-package treemacs-magit
  :after (treemacs magit))

;; Terminal emulator
(use-package vterm)

;; Claude Code IDE integration
;; Key binding: C-c C-'
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; ============================================================================
;;; Language-Specific Modes
;;; ============================================================================

;;; Lisp Languages
(use-package paredit
  :hook ((lisp-data-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

;;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

;;; TypeScript/TSX
;; Uses built-in tree-sitter modes for better syntax highlighting and navigation
;; Automatically installs tree-sitter grammars if not present
;; Eglot provides LSP features (requires typescript-language-server installed)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  ;; Define grammar sources for tree-sitter
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
  ;; Install grammars if not available
  (unless (treesit-language-available-p 'typescript)
    (treesit-install-language-grammar 'typescript))
  (unless (treesit-language-available-p 'tsx)
    (treesit-install-language-grammar 'tsx)))

;;; Ruby
(use-package inf-ruby
  :commands inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

;; Ruby documentation lookup
(use-package yari
  :commands yari)

;;; Docker
(use-package dockerfile-mode)

(use-package docker
  :commands docker)

;;; Document Formats
;; EPUB reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;;; AI Assistant
(use-package gptel
  :commands (gptel gptel-send))

(provide 'init)
;;; init.el ends here
