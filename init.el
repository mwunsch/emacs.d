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
(add-hook 'prog-mode-hook #'linum-mode)

;; Ido and Completion
(ido-mode 1)
(ido-everywhere 1)
(use-package ido-completing-read+
  :init (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :init (ido-vertical-mode 1)
  :custom (ido-vertical-define-keys 'C-n-C-p-up-and-down))

;; Cosmetic
(when (member "SF Mono" (font-family-list))
  (set-face-attribute 'default nil :family "SF Mono" :height 124))

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package solaire-mode
  :config (solaire-global-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-icon nil))

(use-package anzu
  :init (global-anzu-mode 1))

;; Projects
(use-package projectile
  :init (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package magit
  :commands magit-status)

(use-package gist
  :commands (gist-list gist-region-or-buffer))

;; Coding
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l"))

(use-package paredit
  :hook ((lisp-data-mode . paredit-mode)
         (scheme-mode . paredit-mode)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package rust-mode
  :mode "\\.rs\\'")
