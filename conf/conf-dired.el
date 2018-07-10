;;; org.el ---

;; Configures dired-mode

(require 'dired )
;; (require 'dired-collapse)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-dire

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; (add-hook 'dired-mode-hook 'dired-collapse-mode)
