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

;; Skeletons
;;
;; sblk - Generate block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; splantuml - PlantUML source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes\n"
  "@startuml\n"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

(define-skeleton skel-org-block-plantuml-activity
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
  "@startuml\n"
  "skinparam activity {\n"
  "BackgroundColor<<New>> Cyan\n"
  "}\n\n"
  "title " str " - \n"
  "note left: " str "\n"
  "(*) --> (*)\n"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

(define-skeleton skel-org-block-plantuml-activity-if
  "Insert a org plantuml block activity if statement"
  "" 
  "if \"\" then\n"
  "  -> [] \"" - _ "\"\n"
  "  --> ==M1==\n"
  "  -left-> ==M2==\n"
  "else\n"
  "end if\n"
  "--> ==M2==")

(define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

(define-skeleton skel-org-block-plantuml-activity-for
  "Insert a org plantuml block activity for statement"
  "" 
  "--> ==LOOP1==\n"
  "note left: Loop1: For each\n"
  "--> ==ENDLOOP1==\n"
  "note left: Loop1: End for each")

(define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

(define-skeleton skel-org-block-plantuml-sequence
  "Insert a org plantuml activity diagram block, querying for filename."
  "File appends (no extension): "
  "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
  "@startuml\n"
  "title " str " - \n"
  "actor CSR as \"Customer Service Representative\"\n"
  "participant CSMO as \"CSM Online\"\n"
  "participant CSMU as \"CSM Unix\"\n"
  "participant NRIS\n"
  "actor Customer"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

;; archive all DONE tasks
(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
