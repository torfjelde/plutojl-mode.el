;;; plutojl-mode.el --- Emacs mode for editing raw Pluto.jl notebooks -*- lexical-binding: t; -*-

;; Author: Tor Erlend Fjelde <tor.github@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.0") (uuidgen "1.2"))
;; Keywords: convenience, languages
;; URL: https://github.com/torfjelde/plutojl-mode.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pluto.jl mode is an Emacs minor mode for editing raw Pluto.jl notebooks.
;; It provides features for convenient editing and interacting with Pluto.jl notebooks.

;;; Code:

(require 'uuidgen)

(defgroup plutojl nil
  "Emacs mode for editing raw Pluto.jl notebooks."
  :group 'languages)

(defcustom plutojl-preserve-position-upon-revert t
  "Whether to preserve the position of the point when reverting the buffer."
  :type 'boolean
  :group 'plutojl)

;; We also want to hook into `auto-revert-mode' in two cases:
;; - Before the buffer is reverted, we want to save the current cell UUID.
;; - After the buffer is reverted, we want to go to the cell with the saved UUID.

(defvar plutojl--auto-revert-saved-cell-uuid-and-position nil
  "The UUID and point offset of the cell that was active before the buffer was reverted.")

(defconst plutojl--cell-uuid-regexp
  "^# ╔═╡ \\([0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\)$")

(defconst plutojl--cell-order-list-regexp
  "^# ╔═╡ Cell order:")

(defconst plutojl--project-toml-regexp
  "^# ╔═╡ 00000000-0000-0000-0000-000000000001$")

(defconst plutojl--manifest-toml-regexp
  "^# ╔═╡ 00000000-0000-0000-0000-000000000002$")

;; TODO: Add support for disabling code blocks, etc.
;; More generally, add support for metadata.

(defun plutojl--make-cell-regexp (uuid)
  "Return a regexp pattern for matching beginning cell with `uuid'."
  (format "^# ╔═╡ %s$" uuid))

(defun plutojl--make-cell-order-list-entry-regexp (uuid)
  "Return a regexp pattern for matching `uuid' in the cell order list."
  (format "^# \\(╟─\\|╠═\\)%s$" uuid))

(defun plutojl--make-cell-order-list-entry (uuid &optional folded)
  "Return a string for inserting `uuid' in the cell order list."
  (format "# %s%s"
          (if folded "╟─" "╠═")
          uuid))

(defun plutojl--go-to-cell-order-list ()
  "Go to the cell order list."
  ;; The cell order list is identified by `# ╔═╡ Cell order:'
  ;; It is a list of UUIDs, one per line.
  (let ((pom (point)))
    (goto-char (point-max))
    (if (re-search-backward plutojl--cell-order-list-regexp nil t)
        (goto-char (match-beginning 0))
      ;; Go back otherwise.
      (progn
        (goto-char pom)
        (error "No cell order list found")))))

(defun plutojl--current-cell-uuid-and-position ()
  "Return the UUID and the starting position of the current cell."
  (save-excursion
    (forward-line)
    (if (re-search-backward plutojl--cell-uuid-regexp nil t)
        ;; Get the match without fontification.
        (list (buffer-substring-no-properties (match-beginning 1) (match-end 1)) (point))
      (error "No cell found at point"))))

(defun plutojl--current-cell-uuid ()
  "Return the UUID of the current cell."
  (car (plutojl--current-cell-uuid-and-position)))

(defun plutojl--insert-cell-order-after (uuid previous-uuid &optional folded)
  "Insert `uuid' in the cell order list after `previous-uuid'."
  ;; Locate `previous-uuid' in the cell order list.
  ;; If it is found, insert `uuid' after it.
  ;; If it is not found, insert `uuid' at the end of the list.
  (save-excursion
    (plutojl--go-to-cell-order-list)
    ;; The list entries are either of the form
    ;; `# ╟─ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; or
    ;; `# ╠═ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; where `╟─'  indicates it's folded and `╠═' indicates it's unfolded.
    (if (re-search-forward (plutojl--make-cell-order-list-entry-regexp previous-uuid) nil t)
        (progn
          (forward-line)
          (insert (plutojl--make-cell-order-list-entry uuid folded))
          (insert "\n"))
      (error "No cell with UUID %s found" previous-uuid))))

(defun plutojl--insert-cell-order-before (uuid next-uuid &optional folded)
  "Insert `uuid' in the cell order list before `next-uuid'."
  ;; Locate `next-uuid' in the cell order list.
  ;; If it is found, insert `uuid' before it.
  ;; If it is not found, insert `uuid' at the end of the list.
  (save-excursion
    (plutojl--go-to-cell-order-list)
    ;; The list entries are either of the form
    ;; `# ╟─ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; or
    ;; `# ╠═ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; where `╟─'  indicates it's folded and `╠═' indicates it's unfolded.
    (if (re-search-forward (plutojl--make-cell-order-list-entry-regexp next-uuid) nil t)
        (progn
          (beginning-of-line)
          (insert (plutojl--make-cell-order-list-entry uuid folded))
          (insert "\n"))
      (error "No cell with UUID %s found" next-uuid))))

(defun plutojl--cell-exists-in-cell-order-p (uuid)
  "Return non-nil if `uuid' exists in the cell order list."
  (save-excursion
    (plutojl--go-to-cell-order-list)
    (re-search-forward (plutojl--make-cell-order-list-entry-regexp uuid) nil t)))

(defun plutojl--add-to-cell-order (uuid &optional previous-uuid next-uuid folded)
  "Add `uuid' to the cell order list."
  ;; Check that `uuid' is not already in the cell order list.
  (when (plutojl--cell-exists-in-cell-order-p uuid)
    (error "Cell with UUID %s already exists" uuid))  

  (if previous-uuid
      (plutojl--insert-cell-order-after uuid previous-uuid folded)
    (if next-uuid
        (plutojl--insert-cell-order-before uuid next-uuid folded)
      ;; Otherwise we go the last line of the buffer and insert the UUID there.
      (progn
        (goto-char (point-max))
        (insert (plutojl--make-cell-order-list-entry uuid folded))
        (insert "\n")))))

(defun plutojl--delete-from-cell-order (uuid)
  "Delete `uuid' from the cell order list."
  (save-excursion
    (plutojl--go-to-cell-order-list)
    (when (re-search-forward (plutojl--make-cell-order-list-entry-regexp uuid) nil t)
      (beginning-of-line)
      (kill-line)
      (kill-line))))

(defun plutojl--find-previous-cell-uuid ()
  "Return the UUID of the previous cell."
  (save-excursion
    (when (re-search-backward plutojl--cell-uuid-regexp nil t)
      ;; Get the match without fontification.
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun plutojl--find-next-cell-uuid ()
  "Return the UUID of the next cell."
  (save-excursion
    (when (re-search-forward plutojl--cell-uuid-regexp nil t)
      ;; Get the match without fontification.
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun plutojl--goto-cell (uuid)
  "Go to the cell with UUID `uuid'."
  (let ((pom (point)))
    (goto-char (point-min))
    (if (re-search-forward (plutojl--make-cell-regexp uuid) nil t)
        (beginning-of-line)
      (progn
        ;; Go back to where we were.
        (goto-char pom)
        (error "No cell with UUID %s found" uuid)))))

(defun plutojl--auto-revert-handler ()
  "Save the current cell UUID before the buffer is reverted."
  (when plutojl-mode
    (let ((uuid-and-pos (plutojl--current-cell-uuid-and-position)))
      ;; NOTE: We can't use `setq-local' because local variables are not preserved
      ;; when the buffer is reverted. If this ever causes issues, we can always make
      ;; these variables a plist, one entry for each buffer.
      (setq plutojl--auto-revert-saved-cell-uuid-and-offset
            (list
             (car uuid-and-pos)
             (- (point) (cadr uuid-and-pos)))))))

(defun plutojl--auto-revert-handler-after ()
  "Go to the cell with the saved UUID after the buffer is reverted."
  (when plutojl-mode
    (when plutojl--auto-revert-saved-cell-uuid-and-offset
      (let ((uuid (car plutojl--auto-revert-saved-cell-uuid-and-offset))
            (offset (cadr plutojl--auto-revert-saved-cell-uuid-and-offset)))
        (plutojl--goto-cell uuid)
        (when offset
          (goto-char (+ (point) offset)))
        ;; Clear the saved UUID and offset.
        (setq plutojl--auto-revert-saved-cell-uuid-and-offset nil)))))

(defun plutojl-insert-cell-at-point (&optional arg)
  "Insert a new cell at point.

With prefix ARG, insert a folded cell.

If region is active, make the region the body of the cell."
  (interactive "P")
  ;; First insert the cell UUID that we're creating.
  (let ((uuid (uuidgen-4))
        (content (if (region-active-p)
                     ;; Yank the region and then delete it.
                     (prog1
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (delete-region (region-beginning) (region-end)))
                   nil)))
    ;; Go to the next cell and insert two newlines before it.
    (plutojl-goto-next-cell)
    (insert "\n\n")
    (forward-line -2)
    ;; Insert the cell UUID.
    (plutojl--add-to-cell-order uuid (plutojl--find-previous-cell-uuid) (plutojl--find-next-cell-uuid) arg)
    (insert "# ╔═╡ " uuid "\n")
    (when content
      (insert content))))

(defun plutojl-insert-org-cell-at-point (&optional arg)
  "Insert a Org cell at point.

With prefix ARG, insert a folded cell.

If region is active, make the region the body of the cell."
  (interactive "P")
  (plutojl-insert-cell-at-point arg)
  (insert "org\"\"\"\n")
  (insert "\n\"\"\"")
  (forward-line -1))

(defun plutojl-insert-markdown-cell-at-point (arg)
  "Insert a Markdown cell at point.

With prefix ARG, insert a folded cell.

If region is active, make the region the body of the cell."
  (interactive "P")
  (plutojl-insert-cell-at-point arg)
  (insert "md\"\"\"\n")
  (insert "\n\"\"\"")
  (forward-line -1))

(defun plutojl-insert-html-cell-at-point (arg)
  "Insert a Markdown cell at point.

With prefix ARG, insert a folded cell.

If region is active, make the region the body of the cell."
  (interactive "P")
  (plutojl-insert-cell-at-point arg)
  (insert "html\"\"\"\n")
  (insert "\n\"\"\"")
  (forward-line -1))

(defun plutojl-goto-previous-cell (arg)
  "Go to the previous cell."
  (interactive "p")
  (when (re-search-backward plutojl--cell-uuid-regexp nil t)
    (goto-char (match-beginning 0)))
  (when (not (= (1- arg) 0))
    (plutojl-goto-previous-cell (1- arg))))

(defun plutojl-goto-next-cell (arg)
  "Go to the previous cell."
  (interactive "p")
  ;; If we're already looking at a cell UUID, we go to the next line
  ;; before we start searching.
  (when (looking-at plutojl--cell-uuid-regexp)
    (forward-line))
  (when (re-search-forward plutojl--cell-uuid-regexp nil t)
    (goto-char (match-beginning 0)))
  ;; In case we've moved one line forward, we go back one line.
  (when (not (looking-at plutojl--cell-uuid-regexp))
    (forward-line -1))
  (when (not (<= (1- arg) 0))
    (plutojl-goto-next-cell (1- arg))))

(defun plutojl-goto-project-toml ()
  "Go to the Project.toml section."
  (interactive)
  (let ((pom (point)))
    (goto-char (point-min))
    (if (re-search-forward plutojl--project-toml-regexp nil t)
        (goto-char (match-beginning 0))
      ;; Go back otherwise.
      (goto-char pom))))

(defun plutojl-goto-manifest-toml ()
  "Go to the Manifest.toml section."
  (interactive)
  (let ((pom (point)))
    (goto-char (point-min))
    (if (re-search-forward plutojl--manifest-toml-regexp nil t)
        (goto-char (match-beginning 0))
      ;; Go back otherwise.
      (goto-char pom))))

(defun plutojl-delete-cell-at-point (arg)
  "Delete the cell at point."
  (interactive "p")
  (save-excursion
      ;; In case the pointer is pointing at a cell UUID, we
      ;; go to the next line before we start searchng backwards.
      ;; This won't cause any issues if the next line is a cell UUID
      ;; since the pointer will be at the beginning of the line.
      (forward-line)
      (when (not (plutojl-goto-previous-cell))
        (error "No cell found at point"))
      (let ((start (point)))
        ;; Extract the UUID of the cell we're deleting.
        (when (not (re-search-forward plutojl--cell-uuid-regexp nil t))
          (error "This shouldn't happen"))
        (let ((uuid (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
          ;; Now we locate the next cell.
          (when (not (plutojl-goto-next-cell 1))
            (when (not (plutojl-goto-cell-order-list))
              (error "No cell or cell order found; is this a Pluto.jl notebook?")))

          (let ((end (point)))
            (delete-region start end)
            (plutojl--delete-from-cell-order-list uuid)))))
  (when (not (= (1- arg) 0))
    (plutojl-delete-cell-at-point (1- arg))))

(defun plutojl-goto-cell-order-list ()
  "Go to the cell order list."
  (interactive)
  (plutojl--go-to-cell-order-list))

(defun plutojl-toggle-fold-cell ()
  "Fold the cell at point."
  (interactive)
  (let ((uuid (plutojl--current-cell-uuid)))
    (save-excursion
      ;; Replace the cell UUID in the cell order list with a folded cell.
      (plutojl-goto-cell-order-list)
      ;; Replace.
      (re-search-forward (plutojl--make-cell-order-list-entry-regexp uuid))
      (beginning-of-line)
      (if (looking-at "^# ╟─[A-Za-z0-9\\-]+$")
          ;; Unfold.
          (progn
            (replace-match (plutojl--make-cell-order-list-entry uuid nil))
            (message "Unfolded cell %s" uuid))
        ;; Fold.
        (progn
          (replace-match (plutojl--make-cell-order-list-entry uuid t))
          (message "Folded cell %s" uuid))))))

(defun plutojl--is-plutojl-notebook ()
  "Return non-nil if the current buffer is a Pluto.jl notebook."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward plutojl--cell-uuid-regexp nil t)))

(defun plutojl-maybe-enable-plutojl-mode ()
  "Enable `plutojl-mode' if the buffer is a Pluto.jl notebook."
  (when (plutojl--is-plutojl-notebook)
    (plutojl-mode 1)))

;; Let's make this a minor mode.
(define-minor-mode plutojl-mode
  "Minor mode for editing Pluto.jl notebooks."
  :lighter " Pluto"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'plutojl-insert-cell-at-point)
            (define-key map (kbd "C-c C-d") 'plutojl-delete-cell-at-point)
            (define-key map (kbd "C-c C-p") 'plutojl-goto-previous-cell)
            (define-key map (kbd "C-c C-n") 'plutojl-goto-next-cell)
            (define-key map (kbd "C-c C-f") 'plutojl-toggle-fold-cell)
            (define-key map (kbd "C-c C-o") 'plutojl-insert-org-cell-at-point)
            (define-key map (kbd "C-c C-m") 'plutojl-insert-markdown-cell-at-point)
            (define-key map (kbd "C-c C-h") 'plutojl-insert-html-cell-at-point)
            map)
  (cond
   ;; This is run when we're deactivating.
   (plutojl-mode
    (when plutojl-preserve-position-upon-revert
      (add-hook 'before-revert-hook #'plutojl--auto-revert-handler nil t)
      (add-hook 'after-revert-hook #'plutojl--auto-revert-handler-after nil t)))

   ;; This is run when we're activating.
   (t
    (when plutojl-preserve-position-upon-revert
      (remove-hook 'before-revert-hook #'plutojl--auto-revert-handler t)
      (remove-hook 'after-revert-hook #'plutojl--auto-revert-handler-after t)))))

(provide 'plutojl-mode)
;;; plutojl-mode.el ends here
