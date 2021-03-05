

(defvar my-escape-hook nil
  "A hook run when C-g is pressed (or ESC).")

(defun my/escape ()
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open
         (abort-recursive-edit))
        ;; run all escape hooks until one succeed
        ((run-hook-with-args-until-success 'my-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; back to the default
        ((keyboard-quit))))

;; (global-set-key [remap keyboard-quit] #'my/escape)
(global-set-key [remap keyboard-escape-quit] #'my/escape)

(add-hook 'my-escape-hook #'my/compilation-close-on-escape-h 'append)

(defun my/compilation-buffer-p (&optional buf)
  "Return non-nil if BUFFER is a Compilation buffer. Default to the current buffer."
  (let ((buf (or buf (current-buffer))))
    (or (string= (buffer-name buf) "*compilation*")
        (string= (buffer-name buf) "*Go Test*")
        (string= (buffer-name buf) "*Gofmt Errors*"))))

(defun my/compilation-buffers ()
  "Returns a list of compilation buffers."
  (cl-remove-if-not #'my/compilation-buffer-p (buffer-list)))

(defun my/compilation-close-on-escape-h ()
  "If called inside compilation buffer, close that buffer.  Otherwise, close all compilation buffers."
  (if (my/compilation-buffer-p)
      (my/compilation-close)
    (my/compilation-close-all)))

(defun my/compilation-close (&optional buf)
  "Close given compilation buffer."
  (interactive)
  (let ((buf (or buf (current-buffer))))
    (when (my/compilation-buffer-p buf)
      (kill-buffer buf))))

(defun my/compilation-close-all ()
  "Close all compilation buffers."
  (interactive)
  (mapc #'kill-buffer (my/compilation-buffers)))

;; (display-buffer-in-side-window (current-buffer) nil)
