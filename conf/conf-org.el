;;; org.el ---

;; Configures org-mode

;;(add-to-list 'org-modules 'org-habit)
(require 'org-habit)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-switchb)

(eval-after-load "org"
  '(progn
     '(require 'ox-org nil t)
     '(require 'ox-md nil t)
     '(require 'ox-confluence nil t)))

;; =============================================================================
;; org-capture

;; set default notes file
(setq org-directory "~/Dropbox/org/tasks")
(setq org-default-notes-file "~/Dropbox/org/tasks/refile.org")

;; Define the custum capture templates
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
        "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
       ("r" "respond" entry (file org-default-notes-file)
        "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
       ("n" "note" entry (file org-default-notes-file)
        "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
       ("m" "Meeting" entry (file org-default-notes-file)
        "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
       ("p" "Phone call" entry (file org-default-notes-file)
        "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
       ("h" "Habit" entry (file org-default-notes-file)
        "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

;; =============================================================================
;; Workflow states

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)
        ("PHONE" :foreground "forest green" :weight bold)))

;; Fast todo selection allows changing from any task todo state to any other state
;; directly by selecting the appropriate key from the fast todo selection key menu.
(setq org-use-fast-todo-selection t)

;; Allows changing todo states with S-left and S-right skipping all of the normal
;; processing when entering or leaving a todo state.
;; This cycles through the todo states but skips setting timestamps and entering
;; notes which is very convenient when all you want to do is fix up the status
;; of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; The triggers break down to the following rules:
;;
;; - Moving a task to CANCELLED adds a CANCELLED tag
;; - Moving a task to WAITING adds a WAITING tag
;; - Moving a task to HOLD adds WAITING and HOLD tags
;; - Moving a task to a done state removes WAITING and HOLD tags
;; - Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
;; - Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
;; - Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

;; track time when item was finished
(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)

;; =============================================================================
;; Refile setup

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

; Targets complete directly
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun my/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'my/verify-refile-target)

;; =============================================================================
;; Agenda

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;; set agenda files
;; (setq org-agenda-file-regexp "\\`[^.#].*[^_]\\.org\\'"
;; org-agenda-files (list org-directory "~/work/planning"))
(setq org-agenda-files (list "~/org/tasks/aginity.org"))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

;; =============================================================================
;; Export

;; do not interpret 'a_b' as subscript, use 'a_{b}' instead
(setq org-use-sub-superscripts '{})
;; set html5 fancy mode
(setq org-html-html5-fancy 't)


;; Notes custom API
;;
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
;; Babel setup

;; configure org-babel in order to be able generate graphics using
;; external packages
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")
(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.18/libexec/plantuml.jar")

;; remove "Validate XHTML 1.0" from html export
(setq org-export-html-validation-link nil)
;; this is for org-mode version 0.8
(setq org-html-validation-link nil)

;; disable annoying underscore-to-subscript feature
(setq org-export-with-sub-superscripts nil)

;;(add-hook 'org-babel-after-execute-hook 'my/display-inline-images 'append)

;; (require 'ob-clojure)
;; (setq org-babel-clojure-backend 'cider)
;; (require 'cider)

;; make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun my/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (shell . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (sql . t)
         (sqlite . t)
         (latex . t)
         ;;(http . t)
         (ledger . t)))

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

;; Setup latex exporting
;;
;;
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}
                 \\usepackage[margin=1.5cm]{geometry}
                 [DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
               ("\\section{%s}" . "\\section{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))

;; A commonly used environment for source code is lstlisting, provided by the LaTeX package listings
(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)

(setq org-latex-listings-options '(("breaklines" "true")))

;; =============================================================================
;; org-ref configuration
;; (require 'org-ref)

;; (setq org-ref-bibliography-notes "~/org/ref/notes.org"
      ;; org-ref-default-bibliography '("~/org/ref/master.bib")
      ;; org-ref-pdf-directory "~/org/ref/pdfs/")


;; =============================================================================
;; Fix duration units
;; (setq org-duration-format '(("d" . nil) ("h" . t) ("min" . t)))
(setq org-duration-units
      `(("min" . 1)
        ("h" . 60)
        ;; eight-hour days
        ("d" . ,(* 60 8))
        ;; five-day work week
        ("w" . ,(* 60 8 5))
        ;; four weeks in a month
        ("m" . ,(* 60 8 5 4))
        ;; work a total of 12 months a year --
        ;; this is independent of holiday and sick time taken
        ("y" . ,(* 60 8 5 4 12))))

;; configure taskjuggler
(require 'ox-taskjuggler)

(add-to-list
 'ivy-completing-read-handlers-alist
 '(org-capture-refile . completing-read-default))

(setq org-src-window-setup 'other-window)

(setq org-babel-python-command "python3")

;; library of Babel
;; inject certain code to be available in the org files
;; this is the only way how to do this for now
;; https://orgmode.org/manual/Library-of-Babel.html#Library-of-Babel
;; (org-babel-lob-ingest "path/to/simpleShExample.org")
