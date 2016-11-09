;;; mivi.el --- Minimal Vi mode -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/mivi
;; Version:   0.0.0
;; Package-Requires: ((undo-tree "0.6.5") (emacs "25"))

;;; Commentary:
;;; Code:

(require 'undo-tree)

(defvar mivi--state 'command)
(defvar mivi--last-find nil)
(defvar mivi--number 1)
(defvar-local mivi-insert-mode nil)
(defvar-local mivi-command-mode nil)
(defvar-local mivi--last-command nil)
(defvar-local mivi--undo-direction 'undo)

(defconst mivi--modes
  '(mivi-insert-mode mivi-command-mode))

(defconst mivi-motion-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "$" #'end-of-line)
    (define-key map "," #'mivi-repeat-find-opposite)
    (define-key map "0" #'mivi-number-or-bol)
    (define-key map ";" #'mivi-repeat-find)
    (define-key map "B" #'mivi-Backward-word)
    (define-key map "E" #'mivi-End-of-word)
    (define-key map "F" #'mivi-Find)
    (define-key map "H" #'mivi-window-top)
    (define-key map "L" #'mivi-window-bottom)
    (define-key map "M" #'mivi-window-middle)
    (define-key map "T" #'mivi-goto-char-backward)
    (define-key map "W" #'mivi-forward-Word)
    (define-key map "^" #'beginning-of-line-text)
    (define-key map "b" #'backward-word)
    (define-key map "e" #'mivi-end-of-word)
    (define-key map "f" #'mivi-find)
    (define-key map "h" #'backward-char)
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
    (define-key map "l" #'forward-char)
    (define-key map "t" #'mivi-goto-char)
    (define-key map "w" #'mivi-forward-word)
    map))

(defconst mivi-command-map
  (let ((map (copy-keymap mivi-motion-map)))
    (define-key map "A" #'mivi-Append)
    (define-key map "G" #'mivi-goto-line)
    (define-key map "I" #'mivi-Insert)
    (define-key map "O" #'mivi-Open)
    (define-key map "X" #'mivi-delete-backward-char)
    (define-key map "a" #'mivi-append)
    (define-key map "i" #'mivi-insert)
    (define-key map "o" #'mivi-open)
    (define-key map "u" #'mivi-undo)
    (define-key map "x" #'mivi-delete-char)
    (define-key map "." #'mivi-repeat)
    (define-key map (kbd "C-e") #'scroll-up-line)
    (define-key map (kbd "C-y") #'scroll-down-line)
    (define-key map (kbd "C-d") #'mivi-scroll-up)
    (define-key map (kbd "C-u") #'mivi-scroll-down)
    (define-key map (kbd "C-f") #'mivi-scroll-screen-up)
    (define-key map (kbd "C-b") #'mivi-scroll-screen-down)
    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'mivi-number))
    map))

(defconst mivi-insert-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] #'mivi-command)
    (define-key map (kbd "C-[") #'mivi-command)
    map))

(defun mivi-nil ()
  (interactive))

;; Motion commands
(defun mivi-Backward-word (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward "[:space:]\n")
    (skip-chars-backward "^[:space:]\n")))

(defun mivi-end-of-word (&optional arg)
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (forward-word arg)
    (unless (= p (point))
      (backward-char))))

(defun mivi-End-of-word (&optional arg)
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (dotimes (_ arg)
      (skip-chars-forward "[:space:]\n")
      (skip-chars-forward "^[:space:]\n"))
    (unless (= p (point))
      (backward-char))))

(defun mivi-find (&optional arg)
  (interactive "p")
  (mivi--find-internal nil arg))

(defun mivi-Find (&optional arg)
  (interactive "p")
  (mivi--find-internal nil (- arg)))

(defun mivi-forward-word (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (skip-syntax-forward "w")
    (skip-syntax-forward "^w")))

(defun mivi-forward-Word (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-forward "^[:space:]\n")
    (skip-chars-forward "[:space:]\n")))

(defun mivi-goto-char (&optional arg)
  (interactive "p")
  (mivi--find-internal t arg))

(defun mivi-goto-char-backward (&optional arg)
  (interactive "p")
  (mivi--find-internal t (- arg)))

(defun mivi-goto-line (&optional arg)
  (interactive "P")
  (let ((n (mivi--numeric-or-default arg 0)))
    (if (> n 0)
        (progn
          (goto-char (point-min))
          (forward-line (1- n)))
      (goto-char (point-max))
      (forward-line n)))
  (back-to-indentation))

(defun mivi-number-or-bol ()
  (interactive)
  (if (eq last-command #'mivi-number)
      (mivi-number 0)
    (forward-line 0)))

(defun mivi-repeat-find (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,till? ,sign ,ch) (mivi--find-internal till? (* sign arg) ch))))

(defun mivi-repeat-find-opposite (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,till? ,sign ,ch) (mivi--find-internal till? (* (- sign) arg) ch))))

(defun mivi-window-bottom (&optional arg)
  (interactive "P")
  (move-to-window-line -1)
  (forward-line (- 1 (mivi--numeric-or-default arg 1)))
  (back-to-indentation))

(defun mivi-window-middle (&optional arg)
  (interactive "P")
  (move-to-window-line nil)
  (when (eobp)
    (let* ((last-line (line-number-at-pos))
           (first-line (progn (move-to-window-line 0)
                              (line-number-at-pos))))
      (forward-line (/ (- last-line first-line) 2))))
  (back-to-indentation))

(defun mivi-window-top (&optional arg)
  (interactive "P")
  (move-to-window-line (1- (mivi--numeric-or-default arg 1)))
  (back-to-indentation))

;; Insert commands
(defun mivi-append ()
  (interactive)
  (unless (eolp)
    (forward-char))
  (mivi--insert-mode))

(defun mivi-Append ()
  (interactive)
  (end-of-line)
  (mivi--insert-mode))

(defun mivi-insert ()
  (interactive)
  (mivi--insert-mode))

(defun mivi-Insert ()
  (interactive)
  (beginning-of-line)
  (mivi--insert-mode))

(defun mivi-open ()
  (interactive)
  (end-of-line)
  (newline 1 nil)
  (mivi--insert-mode))

(defun mivi-Open ()
  (interactive)
  (forward-line 0)
  (newline 1 nil)
  (forward-line -1)
  (mivi--insert-mode))

;; Scroll commands
(defun mivi-scroll-down (&optional arg)
  (interactive "P")
  (scroll-down (mivi--numeric-or-default arg (/ (window-body-height) 2))))

(defun mivi-scroll-screen-down (&optional arg)
  (interactive "p")
  (scroll-down (* (window-body-height) (prefix-numeric-value arg))))

(defun mivi-scroll-screen-up (&optional arg)
  (interactive "p")
  (scroll-up (* (window-body-height) (prefix-numeric-value arg))))

(defun mivi-scroll-up (&optional arg)
  (interactive "P")
  (scroll-up (mivi--numeric-or-default arg (/ (window-body-height) 2))))

;; Other commands
(defun mivi-command ()
  (interactive)
  (unless (bolp)
    (backward-char))
  (set-frame-parameter nil 'cursor-type 'box)
  (setq mivi--last-command nil)
  (mivi--switch-mode 'mivi-command-mode))

(defun mivi-delete-char (&optional arg)
  (interactive "p")
  (delete-char (mivi--numeric-or-default arg 1)))

(defun mivi-delete-backward-char (&optional arg)
  (interactive "p")
  (delete-char (- (mivi--numeric-or-default arg 1))))

(defun mivi-number (&optional n)
  (interactive)
  (unless n
    (setq n (string-to-number (this-command-keys))))
  (unless (eq last-command #'mivi-number)
    (setq mivi--number 0))
  (setq mivi--number (+ (* mivi--number 10) n))
  (setq prefix-arg mivi--number))

(defun mivi-repeat ()
  (interactive)
  (if (member last-command '(mivi-undo mivi-repeat))
      (if (eq mivi--undo-direction 'undo)
          (undo-tree-undo)
        (undo-tree-redo))
    (call-interactively last-command)))

(defun mivi-undo ()
  (interactive)
  (if (memq mivi--last-command '(mivi-undo mivi-repeat))
      (if (eq mivi--undo-direction 'undo)
          (progn
            (undo-tree-redo)
            (setq mivi--undo-direction 'redo))
        (undo-tree-undo)
        (setq mivi--undo-direction 'undo))
    (undo-tree-undo)
    (setq mivi--undo-direction 'undo))
  (setq mivi--last-command 'mivi-undo))

;; Internal functions
(defun mivi--find-internal (till? &optional arg ch)
  (interactive "p")
  (let ((ch (or ch (read-char (if till? "t-" "f-"))))
        (sign (if (> arg 0) 1 -1))
        (move? (and (> arg 0) (not (eobp)))))
    (when move?
      (forward-char sign))
    (when (search-forward (char-to-string ch) nil t arg)
      (when till?
        (forward-char (- sign))))
    (when move?
      (forward-char (- sign)))
    (unless (memq this-command '(mivi-repeat-find mivi-repeat-find-opposite))
      (setq mivi--last-find (list till? sign ch)))))

(defun mivi--insert-mode ()
  (set-frame-parameter nil 'cursor-type 'bar)
  (mivi--switch-mode 'mivi-insert-mode))

(defun mivi--numeric-or-default (arg &optional default)
  (if (not arg)
      (or default 0)
    (prefix-numeric-value arg)))

(defun mivi--switch-mode (mode)
  (dolist (m mivi--modes)
    (set m (eq m mode))))

(defvar mivi-mode-map-alist
  (list
   (cons 'mivi-insert-mode mivi-insert-map)
   (cons 'mivi-command-mode mivi-command-map)))

(define-minor-mode mivi-local-mode "MiVi command"
  :init-value nil
  (if mivi-local-mode
    (progn
      (setq mivi-command-mode t)
      (setq-local emulation-mode-map-alists
                  (cons mivi-mode-map-alist
                        emulation-mode-map-alists)))))

(defun mivi-local-mode-on ()
  (when (or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
    (mivi-local-mode 1)))

(define-globalized-minor-mode mivi-mode mivi-local-mode mivi-local-mode-on)

(provide 'mivi)
;;; mivi.el ends here
