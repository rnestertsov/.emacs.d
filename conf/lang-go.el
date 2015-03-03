;;; lang-go.el

;; Go language configuration
;;
;; Following externals tools are uses and should be installed separatelly:
;; - goimports http://godoc.org/golang.org/x/tools/cmd/goimports
;; - gocode https://github.com/nsf/gocode

(require 'company-go)

(defun my/go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  ; customize compile command
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'my/go-mode-hook)

