;;; org.el ---

;; Configures org-mode

(setq org-agenda-files (list "c:/Users/rnestertsov/My Box Files/Roman Nestertsov/org/work.org"
                             "c:/Users/rnestertsov/My Box Files/Roman Nestertsov/org/home.org"))

(global-set-key "\C-ca" 'org-agenda)

(defun my/notes ()
  "switch to my org notes dir."
  (interactive)
  (find-file "c:/Users/rnestertsov/Box Sync/Roman Nestertsov/org/"))

;; =============================================================================
;; Workflow states
;; (setq org-todo-keywords
;;       '((sequence "TODO" "STARTED" "|" "DONE")))

;; (setq org-todo-keyword-faces
;;       '(("TODO" . org-warning)
;;         ("STARTED" . "yellow")
;;         ("DONE" . "green"))) 

;; =============================================================================
;; Babel setup

;; configure org-babel in order to be able generate graphics using
;; external packages
(setq org-ditaa-jar-path "c:/bin/ditaa0_9.jar")
(setq org-plantuml-jar-path "c:/bin/plantuml.jar")

;; remove "Validate XHTML 1.0" from html export
(setq org-export-html-validation-link nil)
;; this is for org-mode version 0.8
(setq org-html-validation-link nil)
(setq org-export-html-)

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

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
(setq skeleton-end-hook nil)

;; archive all DONE tasks
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
