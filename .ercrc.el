;;; Like erc notifications mode, but for OSX.
(when (fboundp 'osx-notify)
  (add-hook 'erc-text-matched-hook (lambda (match-type nickuserhost msg)
                                     (when (eq match-type 'current-nick)
                                       (let ((nick (nth 0 (erc-parse-user nickuserhost))))
                                         (when (and (not (string-match-p "^Server:" nick))
                                                    (not (string-match-p "^NickServ" nick)))
                                           (erc-remove-text-properties-region 0 (length msg) msg)
                                           (osx-notify (format "ERC: %s" nick) msg)))))))
