;; Initialize packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Defaults
(use-package better-defaults)
(delete-selection-mode 1)
(setq create-lockfiles nil)
(column-number-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Ido and Completion
(ido-mode 1)
(ido-everywhere 1)
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :config (ido-vertical-mode 1)
  :custom (ido-vertical-define-keys 'C-n-C-p-up-and-down))

;; Cosmetic
(when (member "SF Mono" (font-family-list))
  (set-face-attribute 'default nil :family "SF Mono" :height 124))

(use-package doom-themes
  :config (load-theme 'doom-one t))

(use-package solaire-mode
  :config (solaire-global-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-icon nil))

(use-package anzu
  :config (global-anzu-mode 1))

;; Projects
(use-package projectile
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package magit
  :commands magit-status)

(use-package gist
  :commands (gist-list gist-region-or-buffer))

(use-package dockerfile-mode)

(use-package docker
  :commands docker)

;; Coding
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l"))

(use-package company
  :hook (prog-mode . company-mode))

(use-package paredit
  :hook ((lisp-data-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package inf-ruby
  :commands inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package yari
  :commands yari)
