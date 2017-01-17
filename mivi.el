;;; mivi.el --- Minimal Vi mode -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/mivi
;; Version:   0.0.0
;; Package-Requires: ((undo-tree "0.6.5") (emacs "25"))

;;; Commentary:
;;; Code:

(require 'undo-tree)

(defgroup mivi nil "Minimal Vi mode."
  :group 'emulations
  :prefix "mivi-")

(defcustom mivi-enabled-derived-modes '(conf-mode prog-mode text-mode)
  "Enable mivi in major modes that derive the specified modes.")

(defcustom mivi-enabled-major-modes '(fundamental-mode)
  "Enable mivi in the specified major modes.")

(defcustom mivi-override-universal-argument-map t
  "Whether to disable \\C-u binding in `universal-argument-map'.")

(defcustom mivi-shift-width 2
  "Shiftwidth by which backward indent moves the current indentation.")

(defvar mivi--current-find-char nil)
(defvar mivi--insert-beginning nil)
(defvar mivi--last-buffer nil)
(defvar mivi--last-command nil)
(defvar mivi--last-find nil)
(defvar mivi--last-search nil)
(defvar mivi--stop-at-eol nil)
(defvar mivi--stop-at-space nil)
(defvar mivi--undo-repeating nil)

(defvar-local mivi-change-state nil)
(defvar-local mivi-command-state nil)
(defvar-local mivi-copy-state nil)
(defvar-local mivi-delete-state nil)
(defvar-local mivi-insert-state nil)

(defvar-local mivi--cursor-type 'box)
(defvar-local mivi--undo-direction 'redo)

(defconst mivi--states
  '(mivi-change-state
    mivi-command-state
    mivi-copy-state
    mivi-delete-state
    mivi-insert-state))

(defconst mivi-motion-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "$" #'end-of-line)
    (define-key map "%" #'mivi-goto-pair)
    (define-key map "," #'mivi-repeat-find-opposite)
    (define-key map "/" #'mivi-search)
    (define-key map "0" #'beginning-of-line)
    (define-key map ";" #'mivi-repeat-find)
    (define-key map "?" #'mivi-search-backward)
    (define-key map "B" #'mivi-Backward-word)
    (define-key map "E" #'mivi-End-of-word)
    (define-key map "F" #'mivi-Find)
    (define-key map "G" #'mivi-goto-line)
    (define-key map "H" #'mivi-window-top)
    (define-key map "L" #'mivi-window-bottom)
    (define-key map "M" #'mivi-window-middle)
    (define-key map "N" #'mivi-search-Next)
    (define-key map "T" #'mivi-goto-char-backward)
    (define-key map "W" #'mivi-forward-Word)
    (define-key map "^" #'back-to-indentation)
    (define-key map "b" #'mivi-backward-word)
    (define-key map "e" #'mivi-end-of-word)
    (define-key map "f" #'mivi-find)
    (define-key map "h" #'backward-char)
    (define-key map "j" #'mivi-next-line)
    (define-key map "k" #'mivi-previous-line)
    (define-key map "l" #'forward-char)
    (define-key map "n" #'mivi-search-next)
    (define-key map "t" #'mivi-goto-char)
    (define-key map "w" #'mivi-forward-word)
    (define-key map (kbd "C-h") #'backward-char)
    map))

(defconst mivi-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mivi-motion-map)
    (define-key map "A" #'mivi-Append)
    (define-key map "C" 'mivi-change-end-of-line)
    (define-key map "D" 'mivi-delete-end-of-line)
    (define-key map "I" #'mivi-Insert)
    (define-key map "J" #'mivi-join)
    (define-key map "O" #'mivi-Open)
    (define-key map "P" #'mivi-Paste)
    (define-key map "R" #'mivi-Replace)
    (define-key map "S" #'mivi-change-line)
    (define-key map "X" #'mivi-kill-backward-char)
    (define-key map "Y" #'mivi-copy-line)
    (define-key map "a" #'mivi-append)
    (define-key map "c" #'mivi-change)
    (define-key map "d" #'mivi-delete)
    (define-key map "i" #'mivi-insert)
    (define-key map "o" #'mivi-open)
    (define-key map "p" #'mivi-paste)
    (define-key map "r" #'mivi-replace-char)
    (define-key map "s" #'mivi-substitute)
    (define-key map "u" #'mivi-undo)
    (define-key map "x" #'mivi-kill-char)
    (define-key map "y" #'mivi-copy)
    (define-key map "~" #'mivi-updown-case)
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
    (define-key map (kbd "C-d") #'mivi-backward-indent)
    (define-key map (kbd "C-h") #'delete-backward-char)
    (define-key map (kbd "C-v") #'quoted-insert)
    (define-key map (kbd "C-w") #'backward-kill-word)
    (define-key map [escape] #'mivi-command)
    map))

(defmacro mivi--derive-key (name map new-state key pre-bindings &rest edit-body)
  (declare (debug (form form form form form form body))
           (indent 5))
  (let ((prefix (concat "mivi-" (symbol-name name) "-")))
    `(let* ((orig-fn (lookup-key mivi-motion-map ,key))
            (orig-name (symbol-name orig-fn))
            (new-fn (intern (concat ,prefix
                                    (if (string-match-p "^mivi-" orig-name)
                                        (substring orig-name 5)
                                      orig-name)))))
       (define-key ,map ,key
         (defalias new-fn
           (lambda ()
             (interactive)
             (unwind-protect
                 (let (,@pre-bindings)
                   (call-interactively orig-fn)
                   ,@edit-body)
               (mivi--switch-state ,new-state))
             (setq this-command new-fn)
             (when (memq (quote ,name) '(change delete))
               (mivi--store-command))))))))

(defconst mivi--motion-0-keys
  '("$" "/" "0" "?" "B" "F" "N" "T" "W" "^" "b" "h" "l" "n" "w" "\C-h"))
(defconst mivi--motion-1-keys '("," ";" "E" "e" "f" "t"))
(defconst mivi--motion-2-keys '("%"))
(defconst mivi--motion-line-keys '("G" "H" "L" "M" "j" "k"))

(defconst mivi-change-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (mivi--derive-key change map 'mivi-insert-state key
                        ((beg (point)) (mivi--stop-at-space t))
        (let ((p (point)))
          (when (/= beg p)
            (kill-region beg p)))))

    (dolist (key mivi--motion-1-keys)
      (mivi--derive-key change map 'mivi-insert-state key
                        ((beg (point)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (kill-region beg (1+ p)))
           ((> beg p)
            (kill-region beg p))))))

    (dolist (key mivi--motion-2-keys)
      (mivi--derive-key change map 'mivi-insert-state key
                        ((beg (point)) (eol (eolp)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (kill-region beg (1+ p)))
           ((> beg p)
            (kill-region (if eol beg (1+ beg)) p))))))

    (dolist (key mivi--motion-line-keys)
      (mivi--derive-key change map 'mivi-insert-state key
                        ((beg (progn (forward-line 0) (point))))
        (forward-line 0)
        (let* ((p (point))
               (pmin (min beg p))
               (pmax (max beg p)))
          (goto-char pmax)
          (forward-line)
          (kill-region pmin (if (eobp) (point) (1- (point))))
          (goto-char pmin))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
    (define-key map "c" #'mivi-change-line)
    map))

(defconst mivi-copy-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (mivi--derive-key copy map 'mivi-command-state key
                        ((beg (point)))
        (let ((p (point)))
          (when (/= beg p)
            (mivi--copy-region beg p))
          (goto-char (min beg p)))))

    (dolist (key mivi--motion-1-keys)
      (mivi--derive-key copy map 'mivi-command-state key
                        ((beg (point)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (mivi--copy-region beg (1+ p)))
           ((> beg p)
            (mivi--copy-region beg p)))
          (goto-char (min beg p)))))

    (dolist (key mivi--motion-2-keys)
      (mivi--derive-key copy map 'mivi-command-state key
                        ((beg (point)) (eol (eolp)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (mivi--copy-region beg (1+ p)))
           ((> beg p)
            (mivi--copy-region (if eol beg (1+ beg)) p)))
          (goto-char (min beg p)))))

    (dolist (key mivi--motion-line-keys)
      (mivi--derive-key copy map 'mivi-command-state key
                        ((beg (point)))
        (let ((end (point)))
          (let* ((p0 (progn (forward-line 0) (point)))
                 (p1 (save-excursion
                       (goto-char beg)
                       (forward-line 0)
                       (point)))
                 (pmin (min p0 p1))
                 (pmax (max p0 p1)))
            (goto-char pmax)
            (forward-line)
            (mivi--copy-region pmin (point))
            (goto-char (min beg end))))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
    (define-key map "y" #'mivi-copy-line)
    map))

(defconst mivi-delete-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (mivi--derive-key delete map 'mivi-command-state key
                        ((beg (point)) (mivi--stop-at-eol t))
        (let ((p (point)))
          (when (/= beg p)
            (kill-region beg p)))))

    (dolist (key mivi--motion-1-keys)
      (mivi--derive-key delete map 'mivi-command-state key
                        ((beg (point)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (kill-region beg (1+ p)))
           ((> beg p)
            (kill-region beg p))))))

    (dolist (key mivi--motion-2-keys)
      (mivi--derive-key delete map 'mivi-command-state key
                        ((beg (point)) (eol (eolp)))
        (let ((p (point)))
          (cond
           ((< beg p)
            (kill-region beg (1+ p)))
           ((> beg p)
            (kill-region (if eol beg (1+ beg)) p))))))

    (dolist (key mivi--motion-line-keys)
      (mivi--derive-key delete map 'mivi-command-state key
                        ((beg (progn (forward-line 0) (point))))
        (forward-line 0)
        (let* ((p (point))
               (pmin (min beg p))
               (pmax (max beg p)))
          (goto-char pmax)
          (forward-line)
          (kill-region pmin (point))
          (unless (= pmin (point-min))
            (goto-char (1- pmin)))
          (back-to-indentation))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
    (define-key map "d" #'mivi-delete-line)
    map))

(defun mivi-nil ()
  (interactive))

;; Motion commands
(defun mivi-backward-word (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward "[:blank:]\n")
    (if (looking-back "[[:alnum:]_]" nil)
        (skip-chars-backward "[:alnum:]_")
      (skip-chars-backward "^[:alnum:][:blank:]\n_"))))

(defun mivi-Backward-word (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward "[:blank:]\n")
    (skip-chars-backward "^[:blank:]\n")))

(defun mivi-end-of-word (&optional arg)
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (dotimes (_ arg)
      (skip-chars-forward "[:blank:]\n")
      (if (looking-at-p "[[:alnum:]_]")
          (skip-chars-forward "[:alnum:]_")
        (skip-chars-forward "^[:alnum:][:blank:]\n_")))
    (unless (= p (point))
      (backward-char))))

(defun mivi-End-of-word (&optional arg)
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (dotimes (_ arg)
      (skip-chars-forward "[:blank:]\n")
      (skip-chars-forward "^[:blank:]\n"))
    (unless (= p (point))
      (backward-char))))

(defun mivi-find (&optional arg)
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "f-"))))
    (mivi--find-internal ch nil arg)
    (setq mivi--last-find (list ch 1 nil))))

(defun mivi-Find (&optional arg)
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "F-"))))
    (mivi--find-internal ch nil (- arg))
    (setq mivi--last-find (list ch -1 nil))))

(defun mivi-forward-word (&optional arg)
  (interactive "p")
  (dotimes (i arg)
    (cond
     ((looking-at-p "[[:alnum:]_]")
      (skip-chars-forward "[:alnum:]_"))
     ((not (looking-at-p "[[:blank:]\n]"))
      (skip-chars-forward "^[:alnum:][:blank:]\n_")))
    (cond
     ((and mivi--stop-at-eol
           (= i (1- arg)))
      (skip-chars-forward "[:blank:]"))
     ((not (and mivi--stop-at-space
                (= i (1- arg))))
      (skip-chars-forward "[:blank:]\n")))))

(defun mivi-forward-Word (&optional arg)
  (interactive "p")
  (dotimes (i arg)
    (skip-chars-forward "^[:blank:]\n")
    (cond
     ((and mivi--stop-at-eol
           (= i (1- arg)))
      (skip-chars-forward "[:blank:]"))
     ((not (and mivi--stop-at-space
                (= i (1- arg))))
      (skip-chars-forward "[:blank:]\n")))))

(defun mivi-goto-char (&optional arg)
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "t-"))))
    (mivi--find-internal ch t arg)
    (setq mivi--last-find (list ch 1 t))))

(defun mivi-goto-char-backward (&optional arg)
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "T-"))))
    (mivi--find-internal ch t (- arg))
    (setq mivi--last-find (list ch -1 t))))

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

(defun mivi-goto-pair ()
  (interactive)
  (let* ((c (and (eolp) (char-before)))
         (syn (and c (char-syntax c))))
    (unless (equal syn ?\))
      (skip-syntax-forward "^()")
      (setq c (char-after))
      (setq syn (and c (char-syntax c))))
    (pcase syn
      (?\( (forward-sexp)
           (backward-char))
      (?\) (unless (eolp)
             (forward-char))
           (backward-sexp)))))

(defun mivi-next-line ()
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively #'next-line))
  (setq this-command #'next-line))

(defun mivi-previous-line ()
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively #'previous-line))
  (setq this-command #'previous-line))

(defun mivi-repeat-find (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,ch ,sign ,till?)
     (mivi--find-internal ch till? (* sign arg)))))

(defun mivi-repeat-find-opposite (&optional arg)
  (interactive "p")
  (pcase mivi--last-find
    (`(,ch ,sign ,till?)
     (mivi--find-internal ch till? (* (- sign) arg)))))

(defun mivi-search (&optional arg)
  (interactive "p")
  (let ((re (read-string "/")))
    (mivi--search-internal re arg 1)
    (setq mivi--last-search (cons re 1))))

(defun mivi-search-backward (&optional arg)
  (interactive "p")
  (let ((re (read-string "?")))
    (mivi--search-internal re arg -1)
    (setq mivi--last-search (cons re -1))))

(defun mivi-search-next (&optional arg)
  (interactive "p")
  (pcase mivi--last-search
    (`(,re . ,sign)
     (mivi--search-internal re arg sign))))

(defun mivi-search-Next (&optional arg)
  (interactive "p")
  (pcase mivi--last-search
    (`(,re . ,sign)
     (mivi--search-internal re arg (- sign)))))

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
  (mivi--switch-state 'mivi-insert-state))

(defun mivi-Append ()
  (interactive)
  (end-of-line)
  (mivi--switch-state 'mivi-insert-state))

(defun mivi-insert (&optional move-only)
  (interactive)
  (unless move-only
    (mivi--switch-state 'mivi-insert-state)
    (setq mivi--insert-beginning (point))
    (mivi--store-command)))

(defun mivi-Insert (&optional move-only)
  (interactive)
  (back-to-indentation)
  (unless move-only
    (mivi--switch-state 'mivi-insert-state)
    (setq mivi--insert-beginning (point))
    (mivi--store-command)))

(defun mivi-open (&optional move-only)
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (unless move-only
    (mivi--switch-state 'mivi-insert-state)
    (setq mivi--insert-beginning (point))
    (mivi--store-command)))

(defun mivi-Open (&optional move-only)
  (interactive)
  (forward-line 0)
  (newline 1 nil)
  (forward-line -1)
  (indent-according-to-mode)
  (unless move-only
    (mivi--switch-state 'mivi-insert-state)
    (setq mivi--insert-beginning (point))
    (mivi--store-command)))

(defun mivi-Replace ()
  (interactive)
  (overwrite-mode 1)
  (mivi--switch-state 'mivi-replace-state))

(defun mivi-substitute (&optional arg)
  (interactive "p")
  (delete-char arg)
  (mivi--switch-state 'mivi-insert-state))

;; Change commands
(defun mivi-change (&optional arg)
  (interactive "P")
  (mivi--switch-state 'mivi-change-state)
  (setq prefix-arg arg))

(defun mivi-change-line (&optional arg)
  (interactive "p")
  (let ((beg (save-excursion
               (forward-line (if (< arg 0) arg 0))
               (point)))
        (end (save-excursion
               (forward-line (if (< arg 0) 1 arg))
               (if (eobp)
                   (point)
                 (1- (point))))))
    (unless (= beg end)
      (kill-region beg end)))
  (mivi--switch-state 'mivi-insert-state))

;; Copy commands
(defun mivi-copy (&optional arg)
  (interactive "P")
  (mivi--switch-state 'mivi-copy-state)
  (setq prefix-arg arg))

(defun mivi-copy-line (&optional arg)
  (interactive "p")
  (let ((beg (save-excursion
               (forward-line (if (< arg 0) arg 0))
               (point)))
        (end (save-excursion
               (forward-line (if (< arg 0) 1 arg))
               (point))))
    (unless (= beg end)
      (mivi--copy-region beg end)))
  (mivi--switch-state 'mivi-command-state))

;; Delete commands
(defun mivi-delete (&optional arg)
  (interactive "P")
  (mivi--switch-state 'mivi-delete-state)
  (setq prefix-arg arg))

(defun mivi-delete-line (&optional arg)
  (interactive "p")
  (let ((beg (save-excursion
               (forward-line (if (< arg 0) arg 0))
               (point)))
        (end (save-excursion
               (forward-line (if (< arg 0) 1 arg))
               (point))))
    (unless (= beg end)
      (kill-region beg end)))
  (when (eobp)
    (forward-line -1))
  (mivi--switch-state 'mivi-command-state))

;; Scroll commands
(defun mivi-scroll-down (&optional arg)
  (interactive "P")
  (condition-case nil
      (scroll-down (mivi--numeric-or-default arg (/ (window-body-height) 2)))
    (error (goto-char (point-min)))))

(defun mivi-scroll-screen-down (&optional arg)
  (interactive "p")
  (condition-case nil
      (scroll-down (* (window-body-height) (prefix-numeric-value arg)))
    (error (goto-char (point-min)))))

(defun mivi-scroll-screen-up (&optional arg)
  (interactive "p")
  (condition-case nil
      (scroll-up (* (window-body-height) (prefix-numeric-value arg)))
    (error (goto-char (point-max)))))

(defun mivi-scroll-up (&optional arg)
  (interactive "P")
  (condition-case nil
      (scroll-up (mivi--numeric-or-default arg (/ (window-body-height) 2)))
    (error (goto-char (point-max)))))

;; Other commands
(defun mivi-backward-indent ()
  (interactive)
  (if (looking-back "^[[:blank:]]*\\([0^]\\)?" nil)
      (let ((pc (match-string 1)))
        (if pc
            (progn
              (delete-char -1)
              (indent-line-to 0)
              (when (and (string= pc "^")
                         (boundp 'indent-line-function))
                (funcall indent-line-function)))
          (let ((column (current-column)))
            (indent-line-to (if (< column mivi-shift-width)
                                0
                              (- column mivi-shift-width))))))
    (delete-char 1)))

(defun mivi-command ()
  (interactive)
  (when (and mivi--insert-beginning
             (< mivi--insert-beginning (point)))
    (setq mivi--last-command
          (plist-put mivi--last-command
                     :content (buffer-substring mivi--insert-beginning (point)))))
  (cond
   ((memq last-command '(mivi-open mivi-Open))
    (indent-to-left-margin))
   ((not (bolp))
    (backward-char)))
  (overwrite-mode -1)
  (mivi--switch-state 'mivi-command-state))

(defun mivi-join (&optional arg)
  (interactive "p")
  (dotimes (_ (if (< arg 3) 1 (1- arg)))
    (forward-line 1)
    (let* ((end (save-excursion (skip-chars-forward "[:blank:]") (point)))
           (beg (save-excursion (skip-chars-backward "[:blank:]\n") (point))))
      (delete-region beg end))
    (insert " ")
    (backward-char)))

(defun mivi-kill-char (&optional arg)
  (interactive "p")
  (mivi--store-command)
  (kill-forward-chars arg))

(defun mivi-kill-backward-char (&optional arg)
  (interactive "p")
  (mivi--store-command)
  (kill-backward-chars arg))

(defun mivi-repeat (&optional arg)
  (interactive "P")
  (let ((command (plist-get mivi--last-command :command)))
    (cond
     ((or (eq last-command 'mivi-undo)
          (and mivi--undo-repeating (eq last-command 'mivi-repeat)))
      (if (eq mivi--undo-direction 'undo)
          (undo-tree-undo)
        (undo-tree-redo))
      (setq mivi--undo-repeating t))
     (mivi--last-command
      (pcase command
        ((or `mivi-insert `mivi-Insert `mivi-open `mivi-Open)
         (let ((m (make-marker))
               (content (plist-get mivi--last-command :content)))
           (when content
             (dotimes (_ (mivi--numeric-or-default arg 1))
               (funcall command t)
               (insert content)
               (when (or (not (marker-position m))
                         (< (marker-position m) (point)))
                 (set-marker m (point))))
             (goto-char (1- (marker-position m)))
             (set-marker m nil))))
        (command
         (let ((this-command command)
               (current-prefix-arg (or arg (plist-get mivi--last-command :prefix)))
               (mivi--current-find-char (car mivi--last-find)))
           (call-interactively command))))
      (setq mivi--undo-repeating nil)))))

(defun mivi-undo ()
  (interactive)
  (if (eq mivi--undo-direction 'undo)
      (progn
        (undo-tree-redo)
        (setq mivi--undo-direction 'redo))
    (undo-tree-undo)
    (setq mivi--undo-direction 'undo)))

(defun mivi-updown-case (&optional arg)
  (interactive "p")
  (dotimes (_ arg)
    (let ((p (point))
          (case-fold-search nil))
      (cond
       ((looking-at-p "[[:lower:]]")
        (upcase-region p (1+ p)))
       ((looking-at-p "[[:upper:]]")
        (downcase-region p (1+ p))))
      (forward-char))))

(defun mivi-paste (&optional arg)
  (interactive "p")
  (cond
   ((string-match-p "\n$" (current-kill 0))
    (forward-line 1)
    (unless (bolp)
      (newline)))
   ((not (eolp))
    (forward-char)))
  (dotimes (_ arg)
    (save-excursion (yank))))

(defun mivi-Paste (&optional arg)
  (interactive "p")
  (when (string-match-p "\n$" (current-kill 0))
   (forward-line 0))
  (save-excursion
    (dotimes (_ arg)
      (yank))))

(defun mivi-replace-char (&optional arg)
  (interactive "p")
  (let ((c (read-key "r-")))
    (when (characterp c)
      (delete-char arg)
      (save-excursion
        (insert-char c arg)))))

;; Internal functions
(defun mivi--copy-region (beg end)
  (kill-new (buffer-substring beg end)))

(defun mivi--find-internal (ch till? count)
  (let ((case-fold-search nil)
        (sign (if (> count 0) 1 -1))
        (move? (and (> count 0) (not (eobp)))))
    (when move?
      (forward-char sign))
    (when (search-forward (char-to-string ch) nil t count)
      (when till?
        (forward-char (- sign))))
    (when move?
      (forward-char (- sign)))))

(defun mivi--numeric-or-default (arg &optional default)
  (if (not arg)
      (or default 0)
    (prefix-numeric-value arg)))

(defun mivi--search-internal (re count sign)
  (let ((case-fold-search nil)
        (origin (point))
        (wrapped nil))
    (if (catch 'break
          (when (and (> sign 0) (not (eobp)))
            (forward-char))
          (while (> count 0)
            (if (re-search-forward re nil t sign)
                (progn
                  (setq count (1- count))
                  (when (and (> sign 0) (not (eobp)))
                    (forward-char)))
              (if wrapped
                  (throw 'break nil)
                (setq wrapped t)
                (goto-char (if (> sign 0)
                               (point-min)
                             (point-max))))))
          t)
        (progn
          (when wrapped
            (message "Search wrapped"))
          (goto-char (match-beginning 0)))
      (goto-char origin))))

(defun mivi--store-command (&optional command)
  (setq mivi--last-command (list :prefix current-prefix-arg
                                 :command (or command this-command))))

(defun mivi--switch-state (state)
  (cond
   ((eq state 'mivi-replace-state)
    (setq mivi--cursor-type '(hbar . 7))
    (setq state 'mivi-insert-state))
   ((eq state 'mivi-insert-state)
    (setq mivi--cursor-type 'bar))
   (t
    (setq mivi--cursor-type 'box)))
  (set-frame-parameter nil 'cursor-type mivi--cursor-type)
  (dolist (s mivi--states)
    (set s (eq s state))))

(defvar mivi-mode-map-alist
  (list
   (cons 'mivi-change-state mivi-change-map)
   (cons 'mivi-command-state mivi-command-map)
   (cons 'mivi-copy-state mivi-copy-map)
   (cons 'mivi-delete-state mivi-delete-map)
   (cons 'mivi-insert-state mivi-insert-map)))

(define-minor-mode mivi-local-mode "MiVi command"
  :init-value nil
  (if mivi-local-mode
      (progn
        (add-hook 'after-change-functions #'mivi--after-change-function nil t)
        (setq mivi-command-state t)
        (setq-local emulation-mode-map-alists
                    (cons 'mivi-mode-map-alist
                          emulation-mode-map-alists)))
    (remove-hook 'after-change-functions #'mivi--after-change-function t)
    (setq emulation-mode-map-alists
          (delete 'mivi-mode-map-alist emulation-mode-map-alists))))

(defun mivi--after-change-function (_beg _end _len)
  (unless undo-in-progress
    (setq mivi--undo-direction 'redo)))

(defun mivi--post-command ()
  (unless (eq mivi--last-buffer (current-buffer))
    (if mivi-local-mode
        (set-frame-parameter nil 'cursor-type mivi--cursor-type)
      (set-frame-parameter nil 'cursor-type 'box))
    (setq mivi--last-buffer (current-buffer))))

(defun mivi-local-mode-on ()
  (when (and (not (minibufferp))
             (or (member major-mode mivi-enabled-major-modes)
                 (catch 'break
                   (dolist (mode mivi-enabled-derived-modes)
                     (when (derived-mode-p mode)
                       (throw 'break t))))))
    (mivi-local-mode 1)))

(defun mivi-local-mode-off ()
  (mivi-local-mode -1))

(defvar mivi-mode nil)

(defun mivi-mode-set (state)
  (if state
      (add-hook 'post-command-hook #'mivi--post-command)
    (remove-hook 'post-command-hook #'mivi--post-command))
  (setq mivi-mode state))

(define-globalized-minor-mode mivi-mode mivi-local-mode mivi-local-mode-on
  :variable (mivi-mode . mivi-mode-set))

(when mivi-override-universal-argument-map
  (define-key universal-argument-map (kbd "C-u") nil))

(with-eval-after-load 'eldoc
  (eldoc-add-command
   "mivi-Backward-word"
   "mivi-End-of-word"
   "mivi-Find"
   "mivi-backward-word"
   "mivi-end-of-word"
   "mivi-find"
   "mivi-forward-Word"
   "mivi-forward-word"
   "mivi-goto-char"
   "mivi-goto-char-backward"
   "mivi-goto-line"
   "mivi-goto-pair"
   "mivi-repeat-find"
   "mivi-repeat-find-opposite"
   "mivi-scroll-down"
   "mivi-scroll-screen-down"
   "mivi-scroll-screen-up"
   "mivi-scroll-up"
   "mivi-window-bottom"
   "mivi-window-middle"
   "mivi-window-top"))

(provide 'mivi)
;;; mivi.el ends here
