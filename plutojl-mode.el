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

(defconst plutojl--cell-uuid-regexp
  "^# ╔═╡ \\([0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\)$")

(defun plutojl--make-cell-order-pattern (uuid)
  "Return a regexp pattern for matching `uuid' in the cell order list."
  (format "^# \\(╟─\\|╠═\\)%s$" uuid))

(defun plutojl--insert-cell-in-order-list-at-point (uuid &optional folded)
  "Insert a cell UUID in the cell order list at point."
  (insert (format "# %s%s\n"
                  (if folded "╟─" "╠═")
                  uuid)))

(defun plutojl--go-to-cell-order ()
  "Go to the cell order list."
  ;; The cell order list is identified by `# ╔═╡ Cell order:'
  ;; It is a list of UUIDs, one per line.
  (goto-char (point-min))
  (if (re-search-forward "^# ╔═╡ Cell order:" nil t)
      (line-beginning-position)
    (error "No cell order list found")))

(defun plutojl--insert-cell-order-after (uuid previous-uuid &optional folded)
  "Insert `uuid' in the cell order list after `previous-uuid'."
  ;; Locate `previous-uuid' in the cell order list.
  ;; If it is found, insert `uuid' after it.
  ;; If it is not found, insert `uuid' at the end of the list.
  (save-excursion
    (plutojl--go-to-cell-order)
    ;; The list entries are either of the form
    ;; `# ╟─ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; or
    ;; `# ╠═ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; where `╟─'  indicates it's folded and `╠═' indicates it's unfolded.
    (if (re-search-forward (plutojl--make-cell-order-pattern previous-uuid) nil t)
        (progn
          (forward-line)
          (plutojl--insert-cell-in-order-list-at-point uuid folded))
      (error "No cell with UUID %s found" previous-uuid))))

(defun plutojl--insert-cell-order-before (uuid next-uuid &optional folded)
  "Insert `uuid' in the cell order list before `next-uuid'."
  ;; Locate `next-uuid' in the cell order list.
  ;; If it is found, insert `uuid' before it.
  ;; If it is not found, insert `uuid' at the end of the list.
  (save-excursion
    (plutojl--go-to-cell-order)
    ;; The list entries are either of the form
    ;; `# ╟─ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; or
    ;; `# ╠═ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; where `╟─'  indicates it's folded and `╠═' indicates it's unfolded.
    (if (re-search-forward (plutojl--make-cell-order-pattern next-uuid) nil t)
        (progn
          (beginning-of-line)
          (plutojl--insert-cell-in-order-list-at-point uuid folded))
      (error "No cell with UUID %s found" next-uuid))))

(defun plutojl--cell-exists-in-cell-order-p (uuid)
  "Return non-nil if `uuid' exists in the cell order list."
  (save-excursion
    (plutojl--go-to-cell-order)
    (re-search-forward (plutojl--make-cell-order-pattern uuid) nil t)))

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
        (plutojl--insert-cell-in-order-list-at-point uuid folded)))))

(defun plutojl--delete-from-cell-order (uuid)
  "Delete `uuid' from the cell order list."
  (save-excursion
    (plutojl--go-to-cell-order)
    (when (re-search-forward (plutojl--make-cell-order-pattern uuid) nil t)
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
    (plutojl--add-to-cell-order uuid (plutojl--find-previous-cell-uuid) (plutojl--find-next-cell-uuid) arg)
    (insert "# ╔═╡ " uuid "\n")
    (when content
      (insert content))))

(defun plutojl-goto-previous-cell ()
  "Go to the previous cell."
  (interactive)
  (when (re-search-backward plutojl--cell-uuid-regexp nil t)
    (goto-char (match-beginning 0))))

(defun plutojl-goto-next-cell ()
  "Go to the previous cell."
  (interactive)
  ;; If we're already looking at a cell UUID, we go to the next line
  ;; before we start searching.
  (when (looking-at plutojl--cell-uuid-regexp)
    (forward-line))
  (when (re-search-forward plutojl--cell-uuid-regexp nil t)
    (goto-char (match-beginning 0))))

(defun plutojl-delete-cell-at-point ()
  "Delete the cell at point."
  (interactive)
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
          (when (not (plutojl-goto-next-cell))
            (when (not (plutojl-goto-cell-order))
              (error "No cell or cell order found; is this a Pluto.jl notebook?")))

          (let ((end (point)))
            (delete-region start end)
            (plutojl--delete-from-cell-order uuid))))))

;; Let's make this a minor mode.
(define-minor-mode plutojl-mode
  "Minor mode for editing Pluto.jl notebooks."
  :lighter " Pluto"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'plutojl-insert-cell-at-point)
            (define-key map (kbd "C-c C-d") 'plutojl-delete-cell-at-point)
            (define-key map (kbd "C-c C-p") 'plutojl-goto-previous-cell)
            (define-key map (kbd "C-c C-n") 'plutojl-goto-next-cell)
            map))

(provide 'plutojl-mode)
;;; plutojl-mode.el ends here
