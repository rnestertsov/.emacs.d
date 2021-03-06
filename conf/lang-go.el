;;; lang-go.el

;; Go language configuration
;;
;; Following externals tools are uses and should be installed separatelly:
;; - goimports http://godoc.org/golang.org/x/tools/cmd/goimports
;; - gocode https://github.com/nsf/gocode

(defun my/go-mode-hook ()
  (require 'company)
  (require 'company-go)

  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  ; shortcuts
  (local-set-key (kbd "C-c C-c") 'compile)

  (let ((map go-mode-map))
        (define-key map (kbd "C-c t p") 'go-test-current-project)
        (define-key map (kbd "C-c t f") 'go-test-current-file)
        (define-key map (kbd "C-c t t") 'go-test-current-test)
        (define-key map (kbd "C-c b t") 'go-test-current-benchmark)
        (define-key map (kbd "C-c b f") 'go-test-current-file-benchmarks)
        (define-key map (kbd "C-c b p") 'go-test-current-project-benchmarks)
        (define-key map (kbd "C-c r") 'go-run)
        (define-key map (kbd "<f1>") 'godoc)
        (define-key map (kbd "M-<f1>") 'godoc-at-point))

  ; customize compile command
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'my/go-mode-hook)
