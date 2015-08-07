(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "https://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(tool-bar-mode -1)
