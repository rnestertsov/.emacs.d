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

  ;; (require 'go-autocomplete)
  ;; (require 'auto-complete-config)
  ;; (ac-config-default)

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  ; shortcuts
  (local-set-key (kbd "C-c C-c") 'compile)

  ; customize compile command
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'my/go-mode-hook)
