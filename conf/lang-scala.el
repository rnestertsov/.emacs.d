;;; lang-scala.el --

;;

(defun ensime-sbt-do-run-main ()
  (interactive)
  (let* ((impl-class
            (or (ensime-top-level-class-closest-to-point)
                (return (message "Could not find top-level class"))))
     (cleaned-class (replace-regexp-in-string "<empty>\\." "" impl-class))
     (command (concat "runMain" " " cleaned-class)))
    (setq last-main command)
    (sbt-command command)))

(defun ensime-sbt-do-run-last-main ()
  (interactive)
  (sbt-command last-main))

(defvar last-main nil "last called main method")

(defun my/scala-hook ()
  (progn
    (local-set-key (kbd "C-c C-b m") 'ensime-sbt-do-run-main)
    (local-set-key (kbd "C-c C-b C-m") 'ensime-sbt-do-run-last-main)))

(add-hook 'scala-mode-hook 'my/scala-hook)
