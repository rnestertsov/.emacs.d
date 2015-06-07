;;; org.el ---

;; Configures org-mode

(setq org-agenda-files (list "~/org/tasks"))

;;(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(defvar my-notes-directory "~/org/notes")

(defun my/notes-list ()
  (interactive)
  (let ((default-directory my-notes-directory))
    (sort
     (directory-files default-directory (not 'absolute) ".+\.org" 'nosort)
     (lambda (a b)
       (time-less-p (nth 5 (file-attributes b))
                    (nth 5 (file-attributes a)))))))

(defun my/file-namify-string (str)
  (downcase (replace-regexp-in-string
             " " "-"
             (replace-regexp-in-string  "\\(^[ ]+\\|[ ]+$\\|[^a-z0-9_ ]+\\)" "" str))))

(defun my/note-filename (note-name)
  (concat my-notes-directory "/" note-name ".org"))

(defun my/new-note (note-name)
  (interactive "MNote name: ")
  (let* ((note-filename (my/note-filename note-name))
        (note-buffer (get-buffer-create note-filename)))
    ))

(defun my/notes ()
  "switch to my org notes dir."
  (interactive)
  (find-file my-notes-directory))

;; =============================================================================
;; Workflow states
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("INPROGRESS" . "yellow")
        ("DONE" . "green"))) 

;; track time when item was finished
(setq org-log-done 'time)

;; =============================================================================
;; Babel setup

;; configure org-babel in order to be able generate graphics using
;; external packages
(setq org-ditaa-jar-path "~/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/plantuml.jar")

;; remove "Validate XHTML 1.0" from html export
(setq org-export-html-validation-link nil)
;; this is for org-mode version 0.8
(setq org-html-validation-link nil)
(setq org-export-html-)

;; disable annoying underscore-to-subscript feature
(setq org-export-with-sub-superscripts nil)

;;(add-hook 'org-babel-after-execute-hook 'my/display-inline-images 'append)

;; make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun my/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (sql . t)
         (latex . t))))

(setq org-src-fontify-natively t)
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use plantuml mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . plantuml)))

;; archive all DONE tasks
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE"
   'file))
