(defvar fv-mode-hook nil)

(defgroup fv-mode nil
  "Minor mode for time management using the FV method"
  :tag "FV"
  :group 'wp
  :prefix "fv-")

(defface fv-mode-chain-item-f
  '((t :background "#ffff99"))
  "Face for chain items in FV mode."
  :group 'fv-mode)

(defcustom fv-mode-marker "."
  "Prefix to mark FV chain items."
  :type 'string
  :group 'fv-mode)

(defun fv-mode-chain-prefix-regexp ()
  (format "^%s" (regexp-opt (list fv-mode-marker))))

(defun fv-mode-chain-item-regexp ()
  "Regular expression for chain items"
  (format "^\\(%s.*\\)$" (regexp-opt (list fv-mode-marker))))

(defvar fv-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-m") 'fv-toggle-item)
    (define-key map (kbd "C-c C-l") 'fv-mark-item)
    (define-key map (kbd "C-c C-u") 'fv-unmark-item)
    (define-key map (kbd "C-c C-o") 'fv-finish-item)
    (define-key map (kbd "C-c C-n") 'fv-next-item)
    (define-key map (kbd "C-c C-p") 'fv-previous-item)
    (define-key map (kbd "C-c C-M-n") 'fv-goto-chain-head)
    (define-key map (kbd "C-c C-k") 'fv-clear-chain)
    map)
  "Keymap for FV minor mode")

(defun fv-toggle-item ()
  (interactive)
  (if (fv-current-line-marked-p)
      (fv-unmark-item)
    (fv-mark-item)))

(defun fv-mark-item ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert fv-mode-marker)))

(defun fv-unmark-item ()
  (interactive)
  (if (fv-current-line-marked-p)
      (save-excursion
        (beginning-of-line)
        (delete-forward-char (length fv-mode-marker)))))

(defun fv-current-line-marked-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at (fv-mode-chain-prefix-regexp))))

(defun fv-finish-item ()
  (interactive)
  (let (empty-line-p)
    (save-excursion
      (end-of-line)
      (setq empty-line-p (= 0 (current-column))))
    (unless empty-line-p
      (progn
        (fv-unmark-item)
        (kill-whole-line)
        (save-excursion
          (end-of-buffer)
          (if (< 0 (current-column))
              (open-line 1))
          (end-of-buffer)
          (yank)
          (end-of-line)
          (backward-char)
          (insert (format-time-string " [%Y-%m-%dT%H:%M:%S]")))))))

(defun fv-clear-chain ()
  (interactive)
  (if (y-or-n-p "Clear chain?")
      (save-excursion
        (beginning-of-buffer)
        (while (fv-next-item)
          (fv-unmark-item)))))

(defun fv-next-item ()
  (interactive)
  (let ((has-next (save-excursion
                    (fv-search-chain-forward))))
    (when has-next
        (fv-search-chain-forward))))

(defun fv-search-chain-forward ()
  (next-line)
  (beginning-of-line)
  (re-search-forward (fv-mode-chain-prefix-regexp) nil t))

(defun fv-previous-item ()
  (interactive)
  (let ((has-next (save-excursion
                    (fv-search-chain-backward))))
    (when has-next
        (fv-search-chain-backward))))

(defun fv-search-chain-backward ()
  (previous-line)
  (end-of-line)
  (re-search-backward (fv-mode-chain-prefix-regexp) nil t))

(defun fv-goto-chain-head ()
  (interactive)
  (let ((has-next (save-excursion
                    (end-of-buffer)
                    (re-search-backward (fv-mode-chain-prefix-regexp)))))
    (when has-next
        (progn
          (end-of-buffer)
          (re-search-backward (fv-mode-chain-prefix-regexp))))))

(defun fv-make-font-lock-keywords ()
  "Highlighting regex for FV mode."
  (list
   `(,(fv-mode-chain-item-regexp) 0 'fv-mode-chain-item-f t)))

(define-minor-mode fv-mode
  "Toggle FV mode.
With a prefix argument ARG, enable FV mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable the mode if ARG
is omitted or nil.

When FV mode is enabled, it will highlight lines starting with
`fv-mode-marker` using the face `fv-mode-chain-item-f`.
FV mode also provides handy commands for making FV chains.
"
  :lighter " FV"
  :group 'fv-mode
  (let ((keywords (fv-make-font-lock-keywords)))
    (if fv-mode
        (font-lock-add-keywords nil keywords)
      (font-lock-remove-keywords nil keywords))
    (font-lock-fontify-buffer)))

(defun turn-on-fv-mode ()
  "turn fv-mode on"
  (interactive)
  (fv-mode 1))

(provide 'fv-mode)
