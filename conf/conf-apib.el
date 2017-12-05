;;; conf-apib.el --

;; Configure API Blueprint configuration

;; setup files ending in “.apib” to open in apib-mode
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; TODO: make a hydra for C-c C-x v & C-c C-x p shortcuts
