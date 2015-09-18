(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "https://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg '(ace-window
               ag
               anzu
               aurora-theme
               better-defaults
               cider
               clojure-mode
               company
               elixir-mode
               evil
               exec-path-from-shell
               flx-ido
               flycheck
               gist
               haskell-mode
               ido-ubiquitous
               ido-vertical-mode
               inf-ruby
               magit
               markdown-mode
               paredit
               projectile
               racket-mode
               scss-mode
               smex
               solarized-theme
               web-mode
               yari))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Set-up $PATH and other ENV vars from bashrc
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

(smex-initialize)
(global-prettify-symbols-mode t)
(global-company-mode t)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(global-anzu-mode 1)
(global-auto-revert-mode 1)
(global-magit-file-mode 1)
(delete-selection-mode t)
(projectile-global-mode)

(when (eq window-system 'mac)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Set company mode so that it is invoked on TAB.
(setq company-idle-delay nil)
(setq require-final-newline t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(advice-add 'recompile :around (lambda (oldfun &rest r)
                                 ;; Change recompile prompts to y-or-n
                                 (letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
                                   (apply oldfun r))))

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-TAB") 'company-complete)

;; Mappings so that ⌘-+, ⌘--, ⌘-0 adjust font size like other Mac apps.
;; The 'text-scale-adjust function knows what to do.
(dolist (key '("s-0" "s-=" "s-+" "s--"))
  (global-set-key (kbd key) 'text-scale-adjust))

(define-key projectile-mode-map (kbd "s-p") 'projectile-find-file)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'css-mode-hook #'linum-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-more-italic t)

(defun mw-theme-light ()
  (interactive)
  (load-theme 'solarized-light t))

(defun mw-theme-dark ()
  (interactive)
  (load-theme 'solarized-dark t))

(mw-theme-light)
