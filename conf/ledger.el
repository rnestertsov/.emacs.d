;;; ledger.el

;;

(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; automatically place any amounts such that their last digit is aligned to the
;; column specified by ledger-post-amount-alignment-column
(setq ledger-post-auto-adjust-amounts 't)
(setq ledger-post-amount-alignment-column 52)

