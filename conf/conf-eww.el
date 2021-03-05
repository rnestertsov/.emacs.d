;; conf-eww.el ---

;; Configures eww

;; Enable smooth scrolling of images by using sliced images.
;; Source: http://thanhvg.blogspot.com/2019/08/hack-on-eww-emacs-bowser-to-enable.html
(with-eval-after-load "shr"
    (defun shr-put-image (spec alt &optional flags)
      "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.
Hack to use `insert-sliced-image' to avoid jerky image scrolling."
      (if (display-graphic-p)
          (let* ((size (cdr (assq 'size flags)))
                 (data (if (consp spec)
                           (car spec)
                         spec))
                 (content-type (and (consp spec)
                                    (cadr spec)))
                 (start (point))
                 (image (cond
                         ((eq size 'original)
                          (create-image data nil t :ascent 100
                                        :format content-type))
                         ((eq content-type 'image/svg+xml)
                          (create-image data 'svg t :ascent 100))
                         ((eq size 'full)
                          (ignore-errors
                            (shr-rescale-image data content-type
                                               (plist-get flags :width)
                                               (plist-get flags :height))))
                         (t
                          (ignore-errors
                            (shr-rescale-image data content-type
                                               (plist-get flags :width)
                                               (plist-get flags :height)))))))
            (when image
              (let* ((image-pixel-cons (image-size image t))
                     (image-pixel-width (car image-pixel-cons))
                     (image-pixel-height (cdr image-pixel-cons))
                     (image-scroll-rows (round (/ image-pixel-height (default-font-height)))))
                ;; When inserting big-ish pictures, put them at the
                ;; beginning of the line.
                (when (and (> (current-column) 0)
                           (> (car (image-size image t)) 400))
                  (insert "\n"))

                (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
                ;; original :
                ;; (if (eq size 'original)
                ;;     (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
                ;;   (insert-image image (or alt "*")))

                (put-text-property start (point) 'image-size size)
                (when (and shr-image-animate
                           (cond ((fboundp 'image-multi-frame-p)
                                  ;; Only animate multi-frame things that specify a
                                  ;; delay; eg animated gifs as opposed to
                                  ;; multi-page tiffs.  FIXME?
                                  (cdr (image-multi-frame-p image)))
                                 ((fboundp 'image-animated-p)
                                  (image-animated-p image))))
                  (image-animate image nil 60))))
            image)
        (insert (or alt "")))))
