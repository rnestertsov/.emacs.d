;;; keyboard.el ---

;; keybindings

;; steve yegges's suggested keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c i") 'imenu)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f2] 'save-buffer)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c e") 'next-error)
(global-set-key (kbd "C-c w") 'previous-error)
