(require 'package)
(dolist (source '(("melpa" . "https://melpa.org/packages/")
                  ("marmalade" . "https://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-selected-packages '(ace-window
                                  ag
                                  anzu
                                  auctex
                                  better-defaults
                                  cider
                                  clojure-mode
                                  coffee-mode
                                  company
                                  csv-mode
                                  discover-my-major
                                  editorconfig
                                  elixir-mode
                                  elm-mode
                                  ensime
                                  espresso-theme
                                  evil
                                  exec-path-from-shell
                                  extempore-mode
                                  flx-ido
                                  flycheck
                                  gist
                                  gotham-theme
                                  haskell-mode
                                  hcl-mode
                                  ido-ubiquitous
                                  ido-vertical-mode
                                  inf-ruby
                                  js2-mode
                                  json-mode
                                  magit
                                  markdown-mode
                                  paredit
                                  processing-mode
                                  projectile
                                  racket-mode
                                  restart-emacs
                                  scribble-mode
                                  scss-mode
                                  smex
                                  solarized-theme
                                  subatomic-theme
                                  web-mode
                                  yaml-mode
                                  yari
                                  yasnippet
                                  zerodark-theme))
(package-install-selected-packages)

(require 'better-defaults)

;; Set-up $PATH and other ENV vars from bashrc
(when (memq window-system '(mac ns))
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
(yas-global-mode 1)
(editorconfig-mode 1)

(when (eq window-system 'mac)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-v") 'yank))

;; Set company mode so that it is invoked on TAB.
(setq company-idle-delay nil)
(setq require-final-newline t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))
(setq coffee-tab-width 2)
(setq cider-repl-tab-command 'company-indent-or-complete-common)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)
(setq elm-tags-on-save t)

(setq create-lockfiles nil)
(setq ispell-program-name "aspell")

;;; I send mail with fastmail.fm!
;;; http://www.fastmail.fm/?STKI=12014933
(setq user-mail-address "mark@markwunsch.com")
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-stream-type 'tls
      smtpmail-smtp-service 465)

;;; ERC Configuration
(setq erc-autojoin-channels-alist '(("freenode.net" "#icymi")))
(setq erc-nick "mwunsch")
(setq erc-quit-reason (lambda (s)
                        ;; Waving Hand Sign + Emoji Modifier Fitzpatrick Type 3
                        (or s "\x1f44b\x1f3fc")))
(when (executable-find ispell-program-name)
  (erc-spelling-mode 1))

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
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

;; Mappings so that ⌘-+, ⌘--, ⌘-0 adjust font size like other Mac apps.
;; The 'text-scale-adjust function knows what to do.
(dolist (key '("s-0" "s-=" "s-+" "s--"))
  (global-set-key (kbd key) 'text-scale-adjust))

(define-key projectile-mode-map (kbd "s-p") 'projectile-find-file)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'racket-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'css-mode-hook #'linum-mode)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'abbrev-mode)
(add-hook 'markdown-mode-hook #'flyspell-mode)
(add-hook 'erc-mode-hook #'abbrev-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'scala-mode-hook #'ensime-mode)
;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
(add-hook 'racket-mode-hook (lambda ()
                              (push '("lambda" . 955) prettify-symbols-alist)))
(add-hook 'elm-mode-hook (lambda ()
                           (push '("->" . ?→) prettify-symbols-alist)))
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-script-padding 0)
                           (setq web-mode-style-padding 0)))
(add-hook 'compilation-filter-hook (lambda ()
                                     (ansi-color-apply-on-region compilation-filter-start
                                                                 (point-max))))

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("Appfile\\'" . hcl-mode))

(add-to-list 'company-backends 'company-elm)

(when (file-accessible-directory-p "/Library/TeX/texbin")
  ;; MacTeX puts executables here. Add to exec-path b/c I'd
  ;; prefer not to have this stuff in my $PATH.
  (add-to-list 'exec-path "/Library/TeX/texbin/"))

(put 'css-indent-offset 'safe-local-variable #'integerp)
(put 'sgml-basic-offset 'safe-local-variable #'integerp)
(put 'js-indent-level 'safe-local-variable #'integerp)
(put 'ruby-indent-level 'safe-local-variable #'integerp)
(put 'whitespace-line-column 'safe-local-variable #'integerp)

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-more-italic t)
(setq subatomic-more-visible-comment-delimiters t)

(defun mw-theme-light ()
  (interactive)
  (load-theme 'solarized-light t))

(defun mw-theme-dark ()
  (interactive)
  (load-theme 'solarized-dark t))

;; I use bbatsov/solarized-emacs in a window-system.
;; See: https://github.com/bbatsov/solarized-emacs/issues/18
(if window-system
    (load-theme 'zerodark t)
  (load-theme 'misterioso t))

(when (fboundp 'mac-do-applescript)
    (defun osx-notify (title text)
      "Use the OS X notification mechanism"
      (mac-do-applescript (format
                           "display notification %S with title \"%s\"" text title))))
