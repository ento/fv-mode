(defvar fv-mode-hook nil)

(defgroup fv-mode nil
  "Major mode for time management using the FV method"
  :tag "FV"
  :group 'wp
  :prefix "fv-")

(defgroup fv-mode-faces nil
  "Faces for syntax highlighting."
  :group 'fv-mode
  :group 'faces)

(defface fv-mode-chain-item-f
  '((t :background "dark sea green"))
  "Face for chain items in FV mode."
  :group 'fv-mode-faces)

(defcustom fv-mode-marker "."
  "Prefix to mark chain items."
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
    map)
  "Keymap for FV major mode")

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

(defun fv-next-item ()
  (interactive)
  (let ((has-next (save-excursion
                    (next-line)
                    (beginning-of-line)
                    (re-search-forward (fv-mode-chain-prefix-regexp)))))
    (when has-next
        (progn
          (next-line)
          (beginning-of-line)
          (re-search-forward (fv-mode-chain-prefix-regexp))))))

(defun fv-previous-item ()
  (interactive)
  (let ((has-next (save-excursion
                    (previous-line)
                    (end-of-line)
                    (re-search-backward (fv-mode-chain-prefix-regexp)))))
    (when has-next
        (progn
          (previous-line)
          (end-of-line)
          (re-search-backward (fv-mode-chain-prefix-regexp))))))

(defun fv-make-font-lock-keywords ()
  "Highlighting regex for FV mode"
  (list
   `(,(fv-mode-chain-item-regexp) . 'fv-mode-chain-item-f)))

(define-derived-mode fv-mode fundamental-mode "FV"
  "Major mode for managing a list of tasks a la FV"
  (set
   (make-local-variable 'font-lock-defaults)
;   '(fv-make-font-lock-keywords t nil)
   (list (fv-make-font-lock-keywords) t)
   ))

(defun my-font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(provide 'fv-mode)
