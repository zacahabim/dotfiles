(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))


(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

;; Push to global-mark-ring before xref jumps
(defun my-push-mark-before-xref (&rest _)
  "Push current position to global-mark-ring before xref navigation."
  (push-mark nil t))

(advice-add 'xref-find-definitions :before #'my-push-mark-before-xref)
(advice-add 'xref-find-references :before #'my-push-mark-before-xref)
(advice-add 'xref-go-back :before #'my-push-mark-before-xref)
(advice-add 'xref-go-forward :before #'my-push-mark-before-xref)
(advice-add 'xref-goto-xref :before #'my-push-mark-before-xref)

(global-set-key (kbd "C-c o") (quote backward-global-mark))
(global-set-key (kbd "C-c i") (quote forward-global-mark))

(provide 'global-mark-navigation)
