;;; mivi.el --- Minimal Vi mode -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/mivi
;; Version:   0.0.0
;; Package-Requires: ((undo-tree "0.6.5") (emacs "25"))

;;; Commentary:
;;; Code:

(require 'undo-tree)

(defgroup mivi nil "Minimal Vi mode."
  :group 'emulations)

(defcustom mivi-override-universal-argument-map t
  "Whether to disable \\C-u binding in `universal-argument-map'.")

(defvar mivi--last-find nil)
(defvar-local mivi-insert-state nil)
(defvar-local mivi-command-state nil)
(defvar-local mivi-delete-state nil)

(defvar-local mivi--last-command nil)
(defvar-local mivi--undo-direction 'undo)

(defconst mivi--states
  '(mivi-delete-state mivi-insert-state mivi-command-state))

(defconst mivi-motion-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "$" #'end-of-line)
    (define-key map "," #'mivi-repeat-find-opposite)
    (define-key map "0" #'beginning-of-line)
    (define-key map ";" #'mivi-repeat-find)
    (define-key map "B" #'mivi-Backward-word)
    (define-key map "E" #'mivi-End-of-word)
    (define-key map "F" #'mivi-Find)
    (define-key map "G" #'mivi-goto-line)
    (define-key map "H" #'mivi-window-top)
    (define-key map "L" #'mivi-window-bottom)
    (define-key map "M" #'mivi-window-middle)
    (define-key map "T" #'mivi-goto-char-backward)
    (define-key map "W" #'mivi-forward-Word)
    (define-key map "^" #'back-to-indentation)
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
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mivi-motion-map)
    (define-key map "A" #'mivi-Append)
    (define-key map "I" #'mivi-Insert)
    (define-key map "O" #'mivi-Open)
    (define-key map "P" #'mivi-Paste)
    (define-key map "X" #'mivi-kill-backward-char)
    (define-key map "a" #'mivi-append)
    (define-key map "d" #'mivi-delete)
    (define-key map "i" #'mivi-insert)
    (define-key map "o" #'mivi-open)
    (define-key map "p" #'mivi-paste)
    (define-key map "u" #'mivi-undo)
    (define-key map "x" #'mivi-kill-char)
    (define-key map "." #'mivi-repeat)
    (define-key map (kbd "C-e") #'scroll-up-line)
    (define-key map (kbd "C-y") #'scroll-down-line)
    (define-key map (kbd "C-d") #'mivi-scroll-up)
    (define-key map (kbd "C-u") #'mivi-scroll-down)
    (define-key map (kbd "C-f") #'mivi-scroll-screen-up)
    (define-key map (kbd "C-b") #'mivi-scroll-screen-down)
    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    map))

(defconst mivi-insert-map
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] #'mivi-command)
    map))

(defmacro mivi--derive-function (prefix new-state orig-fn pre-form &rest edit-body)
  (declare (debug (form form form form body))
           (indent 4))
  `(let* ((orig-name (symbol-name ,orig-fn))
          (new-fn (intern (concat ,prefix
                                  (if (string-match-p "^mivi-" orig-name)
                                      (substring orig-name 5)
                                    orig-name)))))
     (defalias new-fn
       (lambda ()
         (interactive)
         (unwind-protect
             (let ((-context ,pre-form))
               (call-interactively ,orig-fn)
               ,@edit-body)
           (mivi--switch-state ,new-state))))))

(defconst mivi-delete-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("b" "B" "F" "h" "l" "T"))
      (define-key map key
        (mivi--derive-function "mivi-delete-" 'mivi-command-state
                               (lookup-key mivi-motion-map key)
                               (point)
          (let ((p (point)))
            (when (/= -context p)
              (kill-region -context p))))))

    (dolist (key '("e" "E" "f" "t"))
      (define-key map key
        (mivi--derive-function "mivi-delete-" 'mivi-command-state
                               (lookup-key mivi-motion-map key)
                               (point)
          (let ((p (point)))
            (when (/= -context p)
              (kill-region -context (1+ p)))))))

    (dolist (key '("G" "j" "k"))
      (define-key map key
        (mivi--derive-function "mivi-delete-" 'mivi-command-state
                               (lookup-key mivi-motion-map key)
                               (progn (forward-line 0) (point))
          (forward-line 0)
          (let* ((p (point))
                 (pmin (min -context p))
                 (pmax (max -context p)))
            (goto-char pmax)
            (forward-line)
            (kill-region pmin (point))
            (unless (= pmin (point-min))
              (goto-char (1- pmin)))
            (back-to-indentation)))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
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

(defun mivi-repeat-find (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,till? ,sign ,ch) (mivi--find-internal till? (* sign arg) ch))))

(defun mivi-repeat-find-opposite (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,till? ,sign ,ch) (mivi--find-internal till? (* (- sign) arg) ch))))

(defun mivi-window-bottom (&optional arg)
  (interactive "p")
  (move-to-window-line -1)
  (forward-line (- 1 arg))
  (back-to-indentation))

(defun mivi-window-middle ()
  (interactive)
  (move-to-window-line nil)
  (when (eobp)
    (let* ((last-line (line-number-at-pos))
           (first-line (progn (move-to-window-line 0)
                              (line-number-at-pos))))
      (forward-line (/ (- last-line first-line) 2))))
  (back-to-indentation))

(defun mivi-window-top (&optional arg)
  (interactive "p")
  (move-to-window-line (1- arg))
  (back-to-indentation))

;; Insert commands
(defun mivi-append ()
  (interactive)
  (unless (eolp)
    (forward-char))
  (mivi--insert-state))

(defun mivi-Append ()
  (interactive)
  (end-of-line)
  (mivi--insert-state))

(defun mivi-insert ()
  (interactive)
  (mivi--insert-state))

(defun mivi-Insert ()
  (interactive)
  (back-to-indentation)
  (mivi--insert-state))

(defun mivi-open ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (mivi--insert-state))

(defun mivi-Open ()
  (interactive)
  (forward-line 0)
  (newline 1 nil)
  (forward-line -1)
  (indent-according-to-mode)
  (mivi--insert-state))

;; Delete commands
(defun mivi-delete (&optional arg)
  (interactive "P")
  (mivi--switch-state 'mivi-delete-state)
  (setq prefix-arg arg))

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
  (cond
   ((memq last-command '(mivi-open mivi-Open))
    (indent-to-left-margin))
   ((not (bolp))
    (backward-char)))
  (set-frame-parameter nil 'cursor-type 'box)
  (setq mivi--last-command nil)
  (mivi--switch-state 'mivi-command-state))

(defun mivi-kill-char (&optional arg)
  (interactive "p")
  (kill-forward-chars (mivi--numeric-or-default arg 1)))

(defun mivi-kill-backward-char (&optional arg)
  (interactive "p")
  (kill-backward-chars (mivi--numeric-or-default arg 1)))

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

(defun mivi-paste (&optional arg)
  (interactive "p")
  (unless (eolp)
    (forward-char))
  (save-excursion
    (dotimes (_ arg)
      (yank))))

(defun mivi-Paste (&optional arg)
  (interactive "p")
  (save-excursion
    (dotimes (_ arg)
      (yank))))

;; Internal functions
(defun mivi--find-internal (till? &optional arg ch)
  (interactive "p")
  (let ((case-fold-search nil)
        (ch (or ch (read-char (if till? "t-" "f-"))))
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

(defun mivi--insert-state ()
  (set-frame-parameter nil 'cursor-type 'bar)
  (mivi--switch-state 'mivi-insert-state))

(defun mivi--numeric-or-default (arg &optional default)
  (if (not arg)
      (or default 0)
    (prefix-numeric-value arg)))

(defun mivi--switch-state (state)
  (dolist (s mivi--states)
    (set s (eq s state))))

(defvar mivi-mode-map-alist
  (list
   (cons 'mivi-delete-state mivi-delete-map)
   (cons 'mivi-insert-state mivi-insert-map)
   (cons 'mivi-command-state mivi-command-map)))

(define-minor-mode mivi-local-mode "MiVi command"
  :init-value nil
  (if mivi-local-mode
      (progn
        (setq mivi-command-state t)
        (setq-local emulation-mode-map-alists
                    (cons 'mivi-mode-map-alist
                          emulation-mode-map-alists)))
    (setq emulation-mode-map-alists
          (delete 'mivi-mode-map-alist emulation-mode-map-alists))))

(defun mivi-local-mode-on ()
  (when (or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
    (mivi-local-mode 1)))

(defun mivi-local-mode-off ()
  (mivi-local-mode -1))

(define-globalized-minor-mode mivi-mode mivi-local-mode mivi-local-mode-on)

(when mivi-override-universal-argument-map
  (define-key universal-argument-map (kbd "C-u") nil))

(provide 'mivi)
;;; mivi.el ends here
