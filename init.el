(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "https://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg '(better-defaults
               exec-path-from-shell
               paredit
               clojure-mode
               cider
               ace-window
               projectile
               flx-ido
               ido-vertical-mode
               company
               anzu
               flycheck
               magit
               evil))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Set-up $PATH and other ENV vars from bashrc
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

(global-prettify-symbols-mode t)
(projectile-global-mode)
(global-company-mode t)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(global-anzu-mode 1)
(global-auto-revert-mode t)
;; Set up keybindings in file-visiting buffers for magit
;;   C-x g    'magit-status
;;   C-c M-g  'magit-file-buffer-popup
;;   C-x M-g  'magit-dispatch-popup
(global-magit-file-buffer-mode 1)
(delete-selection-mode t)

;; Set company mode so that it is invoked on TAB.
(setq company-idle-delay nil)
(setq require-final-newline t)

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
;; Mappings so that ⌘-+, ⌘--, ⌘-0 adjust font size like other Mac apps.
;; The 'text-scale-adjust function knows what to do.
(dolist (key '("s-0" "s-=" "s-+" "s--"))
  (global-set-key (kbd key) 'text-scale-adjust))

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
