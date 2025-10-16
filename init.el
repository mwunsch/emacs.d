;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

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

;; Programming mode enhancements
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
  :config (load-theme 'doom-one t))

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

;; Project management with projectile
;; Key bindings: C-c p (projectile-command-map)
(use-package projectile
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Inherit PATH and environment from shell (important on macOS)
;; Configured to read .zshenv for Volta (node version manager)
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-shell-name "/bin/zsh")
  :config
  (exec-path-from-shell-initialize))

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

;; LSP (Language Server Protocol) support
;; Provides IDE-like features: go-to-definition, find-references, etc.
;; Key prefix: C-c l
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l"))

;; On-the-fly syntax checking
;; Automatically enabled in all programming modes
;; Key prefix: C-c !
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; In-buffer completion
;; Navigate with M-n/M-p, complete with RET
(use-package company
  :hook (prog-mode . company-mode))

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
;; LSP enabled for type checking, completion, and refactoring
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred))
  :config
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

;;; AI Assistant
(use-package gptel
  :commands (gptel gptel-send))

(provide 'init)
;;; init.el ends here
