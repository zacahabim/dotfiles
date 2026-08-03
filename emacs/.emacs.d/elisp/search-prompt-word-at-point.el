;;; search-prompt-word-at-point.el --- isearch-style input for search prompts -*- lexical-binding: t; -*-

;; Gives grep/rg/git-grep-style search prompts two isearch-like
;; conveniences, mirroring how isearch itself starts from a region or
;; lets you pull in text with `C-w':
;; - If a region is active in the invoking buffer, its text pre-fills
;;   the prompt (selected, so typing replaces it).
;; - Otherwise, the first `C-w' in the prompt pulls the compound word
;;   at point from the buffer the search was started from -- same
;;   definition as this config's `isearch-yank-compound-word-then-words'
;;   (letters, digits, `_', `-', so camelCase/snake_case/kebab-case
;;   identifiers are grabbed whole). Subsequent presses extend forward
;;   word-by-word (or char-by-char across other punctuation/
;;   whitespace), same idea as isearch's own repeated `C-w'.
;; Only active for prompts explicitly wrapped with
;; `my-search-prompt-with-word-at-point', so normal minibuffer `C-w'
;; (kill-region) and empty prompts elsewhere are unaffected.

(defvar my-search-prompt--source-buffer nil
  "Buffer the current search prompt was invoked from.")

(defvar my-search-prompt--source-point nil
  "Point in the source buffer to yank words forward from.")

(defvar my-search-prompt--first-pull-done nil
  "Non-nil once the first `C-w' press (compound-word grab) has
happened for the current prompt invocation.")

(defconst my-search-prompt--compound-word-chars "a-zA-Z0-9_-"
  "Character set for a \"compound word\" (camelCase/snake_case/kebab-case
identifier), matching this config's `isearch-yank-compound-word-then-words'.")

(defun my-search-prompt-yank-word-at-point (&optional arg)
  "Pull text from the source buffer into the minibuffer.
First press: grab the whole compound word at point (camelCase,
snake_case, kebab-case treated as one word), like this config's
isearch `C-w' does. Subsequent presses: extend forward word-by-word
(via `forward-word'), or char-by-char across other punctuation and
whitespace, like `isearch-yank-word-or-char'. Never crosses a
newline, since these prompts take a single-line pattern."
  (interactive "p")
  (when (buffer-live-p my-search-prompt--source-buffer)
    (let ((arg (or arg 1))
          text)
      (with-current-buffer my-search-prompt--source-buffer
        (save-excursion
          (goto-char my-search-prompt--source-point)
          (if (not my-search-prompt--first-pull-done)
              ;; First press: grab the compound word at point.
              (let* ((line-end (line-end-position))
                     (end (progn (skip-chars-forward my-search-prompt--compound-word-chars line-end)
                                 (point)))
                     (start (progn (skip-chars-backward my-search-prompt--compound-word-chars
                                                         (line-beginning-position))
                                   (point))))
                (setq text (buffer-substring-no-properties start end))
                (setq my-search-prompt--source-point end)
                (setq my-search-prompt--first-pull-done t))
            ;; Subsequent presses: extend forward word-by-word/char-by-char.
            (let ((start (point))
                  (line-end (line-end-position)))
              (dotimes (_ arg)
                (when (< (point) line-end)
                  (if (or (memq (char-syntax (or (char-after) 0)) '(?w))
                          (memq (char-syntax (or (char-after (1+ (point))) 0)) '(?w)))
                      (forward-word 1)
                    (forward-char 1))
                  (when (> (point) line-end)
                    (goto-char line-end))))
              (setq text (buffer-substring-no-properties start (point)))
              (setq my-search-prompt--source-point (point))))))
      (when (and text (> (length text) 0))
        (insert text)))))

(defvar my-search-prompt--active nil
  "Non-nil while a `my-search-prompt-with-word-at-point' form is reading
from the minibuffer, so the setup hook knows to bind `C-w'.")

(defun my-search-prompt--minibuffer-setup ()
  "Locally bind `C-w' to isearch-style word-at-point yanking, without
touching `minibuffer-local-map' globally or for other prompts."
  (when my-search-prompt--active
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map (current-local-map))
       (define-key map (kbd "C-w") #'my-search-prompt-yank-word-at-point)
       map))))

(add-hook 'minibuffer-setup-hook #'my-search-prompt--minibuffer-setup)

(defun my-search-prompt-region-or-nil ()
  "Return the active region's text in the current buffer, or nil."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defmacro my-search-prompt-with-word-at-point (&rest body)
  "Run BODY (a minibuffer-reading form, e.g. `read-string' or
`read-regexp') with `C-w' locally bound to pull the word at point
from the buffer BODY was invoked from, isearch-style, instead of the
default kill-region. Has no effect on the prompt's initial input --
combine with `my-search-prompt-region-or-nil' at the call site to
pre-fill from an active region, mirroring isearch's own behavior.
Works regardless of which read primitive BODY uses internally, since
the `C-w' binding hooks `minibuffer-setup-hook' rather than depending
on an explicit keymap argument."
  `(let* ((my-search-prompt--source-buffer (current-buffer))
          (my-search-prompt--source-point (point))
          (my-search-prompt--first-pull-done nil)
          (my-search-prompt--active t))
     ,@body))

(provide 'search-prompt-word-at-point)
