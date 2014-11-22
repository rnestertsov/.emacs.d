;;; emacs-conf-erc.el --

;; Configuration for ERC irc client
(require 'erc)

;; disable some notices
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; automatically connect to specified channells whenever connected to
;; freenode
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#clojure" "#emacs")))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

;; enable spell checking
(erc-spelling-mode 1)

;; enable spell checking
(erc-spelling-mode 1)

(defun my/start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick "rnestertsov")))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun my/stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))
