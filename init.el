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
               magit
               evil))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Set-up $PATH and other ENV vars from bashrc
(when (eq window-system 'ns)
  (exec-path-from-shell-initialize))

;; Mappings so that ⌘-+, ⌘--, ⌘-0 adjust font size like other Mac apps.
;; The 'text-scale-adjust function knows what to do.
(dolist (key '("s-0" "s-=" "s-+" "s--"))
  (global-set-key (kbd key) 'text-scale-adjust))

(global-set-key (kbd "M-p") 'ace-window)

;; Set up keybindings in file-visiting buffers for magit
;;   C-x g    'magit-status
;;   C-c M-g  'magit-file-buffer-popup
;;   C-x M-g  'magit-dispatch-popup
(global-magit-file-buffer-mode 1)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)



