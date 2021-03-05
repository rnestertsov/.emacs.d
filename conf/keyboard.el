;;; keyboard.el ---

;; keybindings

;; keybindings I used to since ancient Turbo Pascal 7 times
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f3>") 'counsel-find-file)
(global-set-key (kbd "M-<f3>") 'kill-buffer)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f4>") 'counsel-dired)

(global-set-key (kbd "<f9>") 'my/projectile-compile-project)
(global-set-key (kbd "<f10>") 'my/projectile-run-project)

;; (global-set-key (kbd "M-0") 'helm-mini)
(global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
(global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
(global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
(global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
(global-set-key (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
(global-set-key (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
(global-set-key (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
(global-set-key (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
(global-set-key (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
(global-set-key (kbd "M--") 'eyebrowse-close-window-config)

(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<home>") 'move-beginning-of-line)


;; steve yegges's suggested keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c i") 'counsel-imenu)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "C-c y") 'counsel-yank-pop)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f2] 'save-buffer)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c e") 'next-error)
(global-set-key (kbd "C-c w") 'previous-error)

(require 'hydra)
(defhydra hydra-window (:color red :hint nil :column 8)
  ("h" windmove-left "Left")
  ("j" windmove-right "Right")
  ("k" windmove-up "Up")
  ("l" windmove-down "Down")

  ("q" nil))

(global-set-key (kbd "C-x w") 'hydra-window/body)


(global-set-key (kbd "C-v") 'vterm-yank)

(use-package which-key
  :defer 10
  :ensure t
  :config
  (progn
    (which-key-mode 1)))
