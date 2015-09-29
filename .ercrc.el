;;; ERC Configuration

(setq erc-autojoin-channels-alist '(("freenode.net" "#icymi")))
(setq erc-nick "mwunsch")

;;; Like erc notifications mode, but for OSX.
;;; Unsure if ercrc.el is the best place for this, and it's just a first stab...
(when (fboundp 'osx-notify)
  (add-hook 'erc-text-matched-hook (lambda (match-type nickuserhost msg)
                                     (when (eq match-type 'current-nick)
                                       (let ((nick (nth 0 (erc-parse-user nickuserhost))))
                                         (osx-notify (format "ERC: %s" nick) msg))))))
