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

(defun plutojl--add-cell-order-after (uuid previous-uuid)
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
    (if (re-search-forward (format "^# \\(╟─\\|╠═\\)%s$" previous-uuid) nil t)
        (progn
          (forward-line)
          (plutojl--insert-cell-in-order-list-at-point uuid))
      (error "No cell with UUID %s found" previous-uuid))))

(defun plutojl--add-cell-order-before (uuid subsequent-uuid)
  "Insert `uuid' in the cell order list before `subsequent-uuid'."
  ;; Locate `subsequent-uuid' in the cell order list.
  ;; If it is found, insert `uuid' before it.
  ;; If it is not found, insert `uuid' at the end of the list.
  (save-excursion
    (plutojl--go-to-cell-order)
    ;; The list entries are either of the form
    ;; `# ╟─ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; or
    ;; `# ╠═ 52f6010d-ad5f-4e29-8d79-7fdf1d8acf92'
    ;; where `╟─'  indicates it's folded and `╠═' indicates it's unfolded.
    (if (re-search-forward (format "^# \\(╟─\\|╠═\\)%s$" subsequent-uuid) nil t)
        (progn
          (beginning-of-line)
          (plutojl--insert-cell-in-order-list-at-point uuid))
      (error "No cell with UUID %s found" subsequent-uuid))))

(defun plutojl--cell-exists-in-cell-order-p (uuid)
  "Return non-nil if `uuid' exists in the cell order list."
  (save-excursion
    (plutojl--go-to-cell-order)
    (re-search-forward (format "^# \\(╟─\\|╠═\\)%s$" uuid) nil t)))

(defun plutojl--add-to-cell-order (uuid &optional previous-uuid subsequent-uuid)
  "Add `uuid' to the cell order list."
  ;; Check that `uuid' is not already in the cell order list.
  (when (plutojl--cell-exists-in-cell-order-p uuid)
    (error "Cell with UUID %s already exists" uuid))  

  (if previous-uuid
      (plutojl--add-cell-order-after uuid previous-uuid)
    (if subsequent-uuid
        (plutojl--add-cell-order-before uuid subsequent-uuid)
      ;; Otherwise we go the last line of the buffer and insert the UUID there.
      (progn
        (goto-char (point-max))
        (plutojl--insert-cell-in-order-list-at-point uuid)))))

(defun plutojl--find-previous-cell ()
  "Return the UUID of the previous cell."
  (save-excursion
    (when (re-search-backward plutojl--cell-uuid-regexp nil t)
      ;; Get the match without fontification.
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun plutojl--find-subsequent-cell ()
  "Return the UUID of the subsequent cell."
  (save-excursion
    (when (re-search-forward plutojl--cell-uuid-regexp nil t)
      ;; Get the match without fontification.
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun plutojl-insert-cell-at-point ()
  "Insert a new cell at point.

If region is active, make the region the body of the cell."
  (interactive)
  ;; First insert the cell UUID that we're creating.
  (let ((uuid (uuidgen-4))
        (content (if (region-active-p)
                     ;; Yank the region and then delete it.
                     (prog1
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (delete-region (region-beginning) (region-end)))
                   nil)))
    (plutojl--add-to-cell-order uuid (plutojl--find-previous-cell) (plutojl--find-subsequent-cell))
    (insert "# ╔═╡ " uuid "\n")
    (when content
      (insert content))))

;; Let's make this a minor mode.
(define-minor-mode plutojl-mode
  "Minor mode for editing Pluto.jl notebooks."
  :lighter " Pluto"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'plutojl-insert-cell-at-point)
            map))

(provide 'plutojl-mode)
;;; plutojl-mode.el ends here
