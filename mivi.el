;;; mivi.el --- Minimal Vi mode -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/mivi
;; Version:   0.0.0
;; Package-Requires: ((cl-lib "1.0") (undo-tree "0.6.5") (emacs "25"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `mivi-mode' provides basic Vi-like editing layer.

;; To setup it, write the following in your init file:
;;
;; (mivi-setup)
;;

;; In default, `mivi-mode' is enabled on `fundamental-mode' or any derived mode
;; of `conf-mode', `prog-mode' and `text-mode'.
;; To enable `mivi-mode' in a major-mode:
;;
;; (add-to-list 'mivi-enabled-major-modes 'some-mode)
;;
;; To enable `mivi-mode' in any mode derived a major-mode:
;;
;; (add-to-list 'mivi-enabled-derived-modes 'some-mode)
;;

;;; Code:

(require 'cl-lib)

(require 'undo-tree)
(require 'mivi-common)
(require 'mivi-ex)

(defgroup mivi nil "Minimal Vi mode."
  :group 'emulations
  :prefix "mivi-")

(defcustom mivi-enabled-derived-modes '(conf-mode prog-mode text-mode)
  "Enable mivi in major modes that derive the specified modes."
  :type '(repeat symbol))

(defcustom mivi-enabled-major-modes '(fundamental-mode)
  "Enable mivi in the specified major modes."
  :type '(repeat symbol))

(defcustom mivi-override-universal-argument-map t
  "Whether to disable Ctrl+u binding in `universal-argument-map'."
  :type 'boolean)

(defcustom mivi-shift-width 2
  "Shiftwidth by which backward indent moves the current indentation."
  :type 'integer)

(defcustom mivi-tty-escape-timeout 0.2
  "Timeout to wait for subsequent input after ESC key on TTY."
  :type 'float)

(defface mivi-search-highlight
  '((((class color) (min-colors 89) (background light))
     (:background "#d6e9ca"))
    (((class color) (min-colors 89) (background dark))
     (:background "#2f5d50")))
  "Search highlight face.")

(defface mivi-mode-line
  '((((class color) (min-colors 89))
     (:foreground "black" :background "#ffdd00"))
    (t :foreground "black" :background "yellow"))
  "Mode line face for MiVi.")

(defvar mivi--change-or-delete nil)
(defvar mivi--current-find-char nil)
(defvar mivi--current-replace-char nil)
(defvar mivi--current-search-string nil)
(defvar mivi--last-command nil)
(defvar mivi--last-find nil)
(defvar mivi--last-find-char-for-repeat nil)
(defvar mivi--last-replace-char nil)
(defvar mivi--stop-at-eol nil)
(defvar mivi--stop-at-space nil)
(defvar mivi--undo-repeating nil)
(defvar mivi--unmatch-throw-error nil)

(defvar-local mivi-change-state nil)
(defvar-local mivi-command-state nil)
(defvar-local mivi-copy-state nil)
(defvar-local mivi-delete-state nil)
(defvar-local mivi-insert-state nil)

(defvar-local mivi--undo-direction 'redo)

(defvar-local mivi--insert-beginning (make-marker))
(defvar-local mivi--insert-end (make-marker))

(defconst mivi--blank-chars "[:blank:]\r")
(defconst mivi--blanknl-chars (concat mivi--blank-chars "\n"))
(defconst mivi--non-blank-chars (concat "^" mivi--blank-chars))
(defconst mivi--non-blanknl-chars (concat "^" mivi--blank-chars "\n"))
(defconst mivi--word-chars "[:alnum:]_"
  "Word characters, not same as \"word\" in Emacs context.")
(defconst mivi--non-blanknlword-chars
  (concat "^" mivi--blanknl-chars mivi--word-chars))

(defconst mivi--blank-regexp (concat "[" mivi--blank-chars "]"))
(defconst mivi--blankline-regexp (concat "^[" mivi--blank-chars "]*$"))
(defconst mivi--blanknl-regexp (concat "[" mivi--blanknl-chars "]"))
(defconst mivi--non-blanknl-regexp (concat "[" mivi--non-blanknl-chars "]"))
(defconst mivi--wordchar-regexp (concat "[" mivi--word-chars "]"))
(defconst mivi--end-of-sentence-regexp
  "\\(\\.[[:blank:]\r\n]+\\|^[[:blank:]\r]*$\\)")

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
    (define-key map "'" #'mivi-goto-mark-line)
    (define-key map "(" #'mivi-previous-sentence)
    (define-key map ")" #'mivi-next-sentence)
    (define-key map "+" #'mivi-next-line-at-bot)
    (define-key map "," #'mivi-repeat-find-opposite)
    (define-key map "-" #'mivi-previous-line-at-bot)
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
    (define-key map "`" #'mivi-goto-mark)
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
    (define-key map "{" #'mivi-previous-paragraph)
    (define-key map "}" #'mivi-next-paragraph)
    (define-key map (kbd "C-a") #'mivi-search-current-word)
    (define-key map (kbd "C-h") #'backward-char)
    (define-key map (kbd "C-m") #'mivi-next-line-at-bot)
    map))

(defconst mivi-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mivi-motion-map)
    (define-key map "&" #'mivi-repeat-subst)
    (define-key map "." #'mivi-repeat)
    (define-key map ":" #'mivi-ex)
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
    (define-key map "m" #'mivi-mark)
    (define-key map "o" #'mivi-open)
    (define-key map "p" #'mivi-paste)
    (define-key map "r" #'mivi-replace-char)
    (define-key map "s" #'mivi-substitute)
    (define-key map "u" #'mivi-undo)
    (define-key map "x" #'mivi-kill-char)
    (define-key map "y" #'mivi-copy)
    (define-key map "~" #'mivi-updown-case)
    (define-key map (kbd "C-e") #'scroll-up-line)
    (define-key map (kbd "C-y") #'scroll-down-line)
    (define-key map (kbd "C-d") #'mivi-scroll-up)
    (define-key map (kbd "C-u") #'mivi-scroll-down)
    (define-key map (kbd "C-f") #'mivi-scroll-screen-up)
    (define-key map (kbd "C-b") #'mivi-scroll-screen-down)
    (define-key map (kbd "C-w") #'mivi-kill-region)
    (define-key map (kbd "M-w") #'mivi-kill-ring-save)
    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    map))

(defconst mivi-insert-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") #'mivi-backward-indent)
    (define-key map (kbd "C-v") #'quoted-insert)
    (define-key map (kbd "C-w") #'mivi-backward-kill-word)
    (define-key map [escape] #'mivi-command)
    map))

(defconst mivi--non-repeatable-commands
  '(mivi-change-goto-mark
    mivi-change-goto-mark-line
    mivi-delete-goto-mark
    mivi-delete-goto-mark-line))

(defmacro mivi--derive-key (state-type
                            motion-type
                            key
                            pre-bindings
                            &rest edit-body)
  "Macro to derive motion key into STATE-TYPE command key.
STATE-TYPE is one of change, copy or delete.
MOTION-TYPE is one of motion-0, motion-1, motion-2 or motion-line.
KEY is keycode to be derived.
PRE-BINDINGS is any binding form that is bound before the motion.
EDIT-BODY is body form to be called after the motion."
  (declare (debug (form form form form body))
           (indent 3))
  (let ((prefix (concat "mivi-" (symbol-name state-type) "-")))
    `(let* ((orig-fn (lookup-key mivi-motion-map ,key))
            (orig-name (symbol-name orig-fn))
            (new-fn (intern (concat ,prefix
                                    (if (string-match-p "^mivi-" orig-name)
                                        (substring orig-name 5)
                                      orig-name)))))
       (defalias new-fn
         (lambda ()
           (interactive)
           (let (new-state
                 (beg ,(if (and (not (eq state-type 'copy))
                                (eq motion-type 'motion-line))
                           `(save-excursion
                              (forward-line 0) (point))
                         `(point)))
                 .
                 ,(unless (eq motion-type 'motion-line)
                    `((region-func ,(if (eq state-type 'copy)
                                        ''mivi--copy-region
                                      ''kill-region)))))
             (unwind-protect
                 (condition-case nil
                     (let* ((mivi--change-or-delete
                             ,(and (memq state-type '(change delete)) t))
                            (mivi--unmatch-throw-error t)
                            ,@pre-bindings)
                       (call-interactively orig-fn)
                       (let ((end (point)))
                         ,@(pcase motion-type
                             ('motion-0 `((when (/= beg end)
                                            (funcall region-func beg end))))
                             ('motion-1 `((cond
                                           ((< beg end)
                                            (funcall region-func beg (1+ end)))
                                           ((> beg end)
                                            (funcall region-func beg end)))))
                             ('motion-2 `((cond
                                           ((< beg end)
                                            (funcall region-func beg (1+ end)))
                                           ((> beg end)
                                            (funcall region-func
                                                     (if eol beg (1+ beg))
                                                     end)))))
                             (_ edit-body))
                         ,(cond
                           ((eq state-type 'change)
                            `(setq new-state 'mivi-insert-state))
                           ((eq state-type 'copy)
                            `(goto-char (min beg end))))))
                     (user-error nil))
               (mivi--switch-state (or new-state 'mivi-command-state))
               (setq this-command new-fn)
               ,(unless (eq state-type 'copy)
                  `(mivi--store-command :category (quote ,state-type))))))))))

(defconst mivi--motion-0-keys
  '("$" "(" ")" "/" "0" "?" "B" "F" "N" "T" "W" "^" "`" "b" "h" "l" "n" "w"
    "{" "}" "\C-a" "\C-h"))
(defconst mivi--motion-1-keys '("," ";" "E" "e" "f" "t"))
(defconst mivi--motion-2-keys '("%"))
(defconst mivi--motion-line-keys
  '(("'" . nil) ("G" . nil) ("H" . nil) ("L" . nil) ("M" . nil)
    ("j" . t) ("k" . t)))

(defconst mivi-change-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (define-key map key
        (mivi--derive-key change motion-0 key ((mivi--stop-at-space t)))))

    (dolist (key mivi--motion-1-keys)
      (define-key map key
        (mivi--derive-key change motion-1 key ())))

    (dolist (key mivi--motion-2-keys)
      (define-key map key
        (mivi--derive-key change motion-2 key ((eol (eolp))))))

    (dolist (kpc mivi--motion-line-keys)
      (let ((key (car kpc)))
        (define-key map key
          (mivi--derive-key change motion-line key ()
            (forward-line 0)
            (setq end (point))
            (let* ((pmin (min beg end))
                   (pmax (max beg end)))
              (goto-char pmax)
              (forward-line)
              (kill-region pmin (if (eobp) (point) (1- (point))))
              (goto-char pmin))))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
    (define-key map "+" 'mivi-change-next-line)
    (define-key map "-" 'mivi-change-previous-line)
    (define-key map "c" #'mivi-change-line)
    (define-key map (kbd "C-m") 'mivi-change-next-line)
    (define-key map [return] 'mivi-change-next-line)
    map))

(defconst mivi-copy-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (define-key map key
        (mivi--derive-key copy motion-0 key ())))

    (dolist (key mivi--motion-1-keys)
      (define-key map key
        (mivi--derive-key copy motion-1 key ())))

    (dolist (key mivi--motion-2-keys)
      (define-key map key
        (mivi--derive-key copy motion-2 key ((eol (eolp))))))

    (dolist (kpc mivi--motion-line-keys)
      (let ((key (car kpc)))
        (define-key map key
          (mivi--derive-key copy motion-line key ()
            (let* ((p0 (progn (forward-line 0) (point)))
                   (p1 (save-excursion
                         (goto-char beg)
                         (forward-line 0)
                         (point)))
                   (pmin (min p0 p1))
                   (pmax (max p0 p1)))
              (goto-char pmax)
              (forward-line)
              (mivi--copy-region pmin (point)))))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map "+" 'mivi-copy-next-line)
    (define-key map "-" 'mivi-copy-previous-line)
    (define-key map [t] #'mivi-command)
    (define-key map "y" #'mivi-copy-line)
    (define-key map (kbd "C-m") 'mivi-copy-next-line)
    (define-key map [return] 'mivi-copy-next-line)
    map))

(defconst mivi-delete-map
  (let ((map (make-sparse-keymap)))
    (dolist (key mivi--motion-0-keys)
      (define-key map key
        (mivi--derive-key delete motion-0 key ((mivi--stop-at-eol t)))))

    (dolist (key mivi--motion-1-keys)
      (define-key map key
        (mivi--derive-key delete motion-1 key () )))

    (dolist (key mivi--motion-2-keys)
      (define-key map key
        (mivi--derive-key delete motion-2 key ((eol (eolp))))))

    (dolist (kpc mivi--motion-line-keys)
      (let ((key (car kpc))
            (preserve-column (cdr kpc)))
        (define-key map key
          (mivi--derive-key delete motion-line key ((column (current-column)))
            (forward-line 0)
            (setq end (point))
            (let* ((pmin (min beg end))
                   (pmax (max beg end)))
              (goto-char pmax)
              (forward-line)
              (kill-region pmin (point))
              (when (eobp)
                (goto-char (1- pmin)))
              (if preserve-column
                  (move-to-column column)
                (back-to-indentation)))))))

    (dotimes (v 9)
      (define-key map (number-to-string (1+ v)) #'digit-argument))
    (define-key map [t] #'mivi-command)
    (define-key map "+" 'mivi-delete-next-line)
    (define-key map "-" 'mivi-delete-previous-line)
    (define-key map "d" #'mivi-delete-line)
    (define-key map (kbd "C-m") 'mivi-delete-next-line)
    (define-key map [return] 'mivi-delete-next-line)
    map))

;; Motion commands
(defun mivi-backward-word (&optional arg)
  "Move backward to the beginning of word.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward mivi--blanknl-chars)
    (if (looking-back mivi--wordchar-regexp nil)
        (skip-chars-backward mivi--word-chars)
      (skip-chars-backward mivi--non-blanknlword-chars))))

(defun mivi-Backward-word (&optional arg)
  "Move backward until encountering non-blank character.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward mivi--blanknl-chars)
    (skip-chars-backward mivi--non-blanknl-chars)))

(defun mivi-end-of-word (&optional arg)
  "Move forward to the end of word.
With ARG, repeat the specified count."
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (dotimes (_ arg)
      (skip-chars-forward mivi--blanknl-chars)
      (if (looking-at-p mivi--wordchar-regexp)
          (skip-chars-forward mivi--word-chars)
        (skip-chars-forward mivi--non-blanknlword-chars)))
    (unless (= p (point))
      (backward-char))))

(defun mivi-End-of-word (&optional arg)
  "Move forward until encountering non-blank character.
With ARG, repeat the specified count."
  (interactive "p")
  (forward-char)
  (let ((p (point)))
    (dotimes (_ arg)
      (skip-chars-forward mivi--blanknl-chars)
      (skip-chars-forward mivi--non-blanknl-chars))
    (unless (= p (point))
      (backward-char))))

(defun mivi-find (&optional arg)
  "Find forward a character from input at the current line.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "f-"))))
    (mivi--find-internal ch nil arg)
    (setq mivi--last-find (list ch 1 nil))
    (when mivi--change-or-delete
      (setq mivi--last-find-char-for-repeat ch))))

(defun mivi-Find (&optional arg)
  "Find backward a charcter from input at the current line.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "F-"))))
    (mivi--find-internal ch nil (- arg))
    (setq mivi--last-find (list ch -1 nil))
    (when mivi--change-or-delete
      (setq mivi--last-find-char-for-repeat ch))))

(defun mivi-forward-word (&optional arg)
  "Move forward to the beginning of the next word.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (i arg)
    (let ((skipped t))
      (cond
       ((looking-at-p mivi--wordchar-regexp)
        (skip-chars-forward mivi--word-chars))
       ((not (looking-at-p mivi--blanknl-regexp))
        (skip-chars-forward mivi--non-blanknlword-chars))
       (t (setq skipped nil)))
      (cond
       ((and mivi--stop-at-eol
             (= i (1- arg))
             skipped)
        (skip-chars-forward mivi--blank-chars))
       ((not (and mivi--stop-at-space
                  (= i (1- arg))))
        (skip-chars-forward mivi--blanknl-chars))))))

(defun mivi-forward-Word (&optional arg)
  "Move forward to the next non-blank character following blank characters.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (i arg)
    (let ((skipped (> (skip-chars-forward mivi--non-blanknl-chars) 0)))
      (cond
       ((and mivi--stop-at-eol
             (= i (1- arg))
             skipped)
        (skip-chars-forward mivi--blank-chars))
       ((not (and mivi--stop-at-space
                  (= i (1- arg))))
        (skip-chars-forward mivi--blanknl-chars))))))

(defun mivi-goto-char (&optional arg)
  "Go forward until encountering a character from input at the current line.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "t-"))))
    (mivi--find-internal ch t arg)
    (setq mivi--last-find (list ch 1 t))
    (when mivi--change-or-delete
      (setq mivi--last-find-char-for-repeat ch))))

(defun mivi-goto-char-backward (&optional arg)
  "Go backward until encountering a character from input at the current line.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((ch (or mivi--current-find-char (read-char "T-"))))
    (mivi--find-internal ch t (- arg))
    (setq mivi--last-find (list ch -1 t))
    (when mivi--change-or-delete
      (setq mivi--last-find-char-for-repeat ch))))

(defun mivi-goto-line (&optional arg)
  "Go to the line specified by ARG, default to the last line."
  (interactive "P")
  (let ((n (mivi--numeric-or-default arg 0)))
    (if (> n 0)
        (progn
          (goto-char (point-min))
          (forward-line (1- n)))
      (goto-char (point-max))
      (forward-line n)))
  (back-to-indentation))

(defun mivi-goto-mark (ch)
  "Go to the point marked by CH."
  (interactive "c")
  (let ((p (mivi--get-mark ch)))
    (when p
      (goto-char p))))

(defun mivi-goto-mark-line (ch)
  "Go to the line marked by CH."
  (interactive "c")
  (let ((p (mivi--get-mark ch)))
    (when p
      (goto-char p)
      (back-to-indentation))))

(defun mivi-goto-pair ()
  "Go to the point at corresponding pair character.

If the current point is a pair character, go to the corresponding pair
character.

If the current point at end of line following a pair character, go to
the corresponding pair character.

Otherwise, find the next pair character and go to the corresponding
pair character."
  (interactive)
  (let* ((origin (point))
         (c (and (eolp) (char-before)))
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
           (backward-sexp))
      (_ (goto-char origin)
         (when mivi--unmatch-throw-error
           (user-error "No matching pair found"))))))

(defun mivi-next-line ()
  "Move to next line with column preserved via `next-line'."
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively #'next-line))
  (setq this-command #'next-line))

(defun mivi-next-line-at-bot (&optional arg)
  "Move to beginning of text in the next line.
With ARG, go to the specified line relative to the current line."
  (interactive "p")
  (forward-line arg)
  (back-to-indentation))

(defun mivi-next-paragraph (&optional arg)
  "Move to next paragraph, i.e. beginning of next blank lines.
With ARG, repeat the specified count."
  (interactive "p")
  (catch 'break
    (dotimes (_ arg)
      (when (mivi--blankline-p 0)
        (skip-chars-forward mivi--blanknl-chars))
      (unless (re-search-forward mivi--blankline-regexp nil t)
        (goto-char (point-max))
        (throw 'break nil)))))

(defun mivi-next-sentence (&optional arg)
  "Move to next sentence.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (_ arg)
    (if (mivi--blankline-p 0)
        (when (re-search-forward mivi--non-blanknl-regexp nil t)
          (backward-char))
      (if (re-search-forward mivi--end-of-sentence-regexp nil t)
          (goto-char (match-end 0))
        (goto-char (point-max))))))

(defun mivi-previous-line ()
  "Move to previous line with column preserved via `previous-line'."
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively #'previous-line))
  (setq this-command #'previous-line))

(defun mivi-previous-line-at-bot (&optional arg)
  "Move to beginning of text in the previous line.
With ARG, go to the specified line backward relative to the current line."
  (interactive "p")
  (forward-line (- arg))
  (back-to-indentation))

(defun mivi-previous-paragraph (&optional arg)
  "Move to previous paragraph, i.e. end of previous blank lines.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (_ arg)
    (when (mivi--blankline-p 0)
      (skip-chars-backward mivi--blanknl-chars))
    (unless (re-search-backward mivi--blankline-regexp nil t)
      (goto-char (point-min)))))

(defun mivi-previous-sentence (&optional arg)
  "Move to previous sentence.
With ARG, repeat the specified count."
  (interactive "p")
  (dotimes (_ arg)
    (skip-chars-backward mivi--blank-chars)
    (let ((blank? (and (not (eobp))
                       (looking-at-p mivi--blankline-regexp))))
      (cond
       ((and (bolp) (not (bobp))
             (not blank?)
             (mivi--blankline-p -1))
        (forward-line -1))
       (t
        (cond
         (blank?
          (skip-chars-backward mivi--blanknl-chars))
         ((and (not (bobp)) (bolp))
          (backward-char)))
        (if (re-search-backward mivi--end-of-sentence-regexp nil t)
            (progn
              (when (looking-at-p "\\.")
                (forward-char))
              (skip-chars-forward mivi--blanknl-chars))
          (goto-char (point-min))))))))

(defun mivi-repeat-find (&optional arg)
  "Repeat the last find.
With ARG, repeat the specified count."
  (interactive "p")
  (pcase mivi--last-find
    (`(,ch ,sign ,till?)
     (mivi--find-internal ch till? (* sign arg)))))

(defun mivi-repeat-find-opposite (&optional arg)
  "Repeat the last find in the opposite direction.
With ARG, repeat the specified count."
  (interactive "p")
  (pcase mivi--last-find
    (`(,ch ,sign ,till?)
     (mivi--find-internal ch till? (* (- sign) arg)))))

(defun mivi-search (&optional arg)
  "Search regexp from input in the current buffer.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((re (or mivi--current-search-string
                (read-regexp "/" (car mivi--last-search)))))
    (if (string= re "")
        (user-error "No previous search pattern")
      (unwind-protect
          (mivi--search-internal re arg 1)
        (setq mivi--last-search (cons re 1))
        (setq this-command 'mivi-search)))))

(defun mivi-search-backward (&optional arg)
  "Search regexp from input backward in the current buffer.
With ARG, repeat the specified count."
  (interactive "p")
  (let ((re (or mivi--current-search-string
                (read-regexp "?" (car mivi--last-search)))))
    (if (string= re "")
        (user-error "No previous search pattern")
      (unwind-protect
          (mivi--search-internal re arg -1)
        (setq mivi--last-search (cons re -1))
        (setq this-command 'mivi-search-backward)))))

(defun mivi-search-current-word ()
  "Search word at the point in the current buffer."
  (interactive)
  (let ((mivi--current-search-string
         (if (use-region-p)
             (let ((s (buffer-substring (region-beginning) (region-end))))
               (unless (string= s "")
                 (regexp-quote s)))
           (skip-syntax-forward "^w_")
           (let ((s (buffer-substring
                     (progn (skip-syntax-backward "w_") (point))
                     (progn (skip-syntax-forward "w_") (point)))))
             (unless (string= s "")
               (concat "\\_<" (regexp-quote s) "\\_>"))))))
    (when mivi--current-search-string
      (deactivate-mark)
      (call-interactively 'mivi-search))))

(defun mivi-search-next (&optional arg)
  "Search the last used regexp in the current buffer.
With ARG, repeat the specified count."
  (interactive "p")
  (pcase mivi--last-search
    (`(,re . ,sign)
     (mivi--search-internal re arg sign))))

(defun mivi-search-Next (&optional arg)
  "Search the last used regexp in the opposite direction.
With ARG, repeat the specified count."
  (interactive "p")
  (pcase mivi--last-search
    (`(,re . ,sign)
     (mivi--search-internal re arg (- sign)))))

(defun mivi-window-bottom (&optional arg)
  "Move the cursor to beginning of text at bottom of window.
With ARG, move to the specified lines above bottom."
  (interactive "p")
  (move-to-window-line -1)
  (forward-line (- 1 arg))
  (back-to-indentation))

(defun mivi-window-middle ()
  "Move the cursor to beginning of text at middle of window."
  (interactive)
  (move-to-window-line nil)
  (when (eobp)
    (let* ((last-line (line-number-at-pos))
           (first-line (progn (move-to-window-line 0)
                              (line-number-at-pos))))
      (forward-line (/ (- last-line first-line) 2))))
  (back-to-indentation))

(defun mivi-window-top (&optional arg)
  "Move the cursor to beginning of text at top of window.
With ARG, move to the specified lines below top."
  (interactive "p")
  (move-to-window-line (1- arg))
  (back-to-indentation))

;; Insert commands
(defun mivi-append (&optional move-only)
  "Append inputted text after the current cursor.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (unless (eolp)
    (forward-char))
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-Append (&optional move-only)
  "Append inputted text at end of the current line.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (end-of-line)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-insert (&optional move-only)
  "Insert inputted text at the current point.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-Insert (&optional move-only)
  "Insert inputted text at the beginning of text in the current line.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (back-to-indentation)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-open (&optional move-only)
  "Open line to inputted text below the current line.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-Open (&optional move-only)
  "Open line to inputted text above the current line.
If MOVE-ONLY is non-nil, not to switch to insert state."
  (interactive)
  (forward-line 0)
  (newline 1 nil)
  (forward-line -1)
  (indent-according-to-mode)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-insert-state)))

(defun mivi-Replace (&optional move-only)
  "Overwrite text with inputted ones.
If MOVE-ONLY is non-nil, not to switch to replace state."
  (interactive)
  (overwrite-mode 1)
  (unless move-only
    (mivi--store-command :category 'insert)
    (mivi--switch-state 'mivi-replace-state)))

(defun mivi-substitute (&optional arg)
  "Substitute character at the current point with inputted text.
With ARG, substitute the specified number of characters."
  (interactive "p")
  (delete-char arg)
  (mivi--store-command :category 'change)
  (mivi--switch-state 'mivi-insert-state))

;; Change commands
(defun mivi-change (&optional arg)
  "Change text covered by subsequent motion with inputted text.
ARG is passed to the motion command."
  (interactive "P")
  (mivi--switch-state 'mivi-change-state)
  (setq prefix-arg arg))

(defun mivi-change-line (&optional arg)
  "Change text from the current point till end of line.
With ARG, change till end of the specified count below the current line.
If ARG is negative, change text from end of the specified count above
the current line till the current point."
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
  (mivi--switch-state 'mivi-insert-state)
  (mivi--store-command :command 'mivi-change-line :category 'change))

;; Copy commands
(defun mivi-copy (&optional arg)
  "Copy text covered by subsequent motion.
ARG is passed to the motion command."
  (interactive "P")
  (mivi--switch-state 'mivi-copy-state)
  (setq prefix-arg arg))

(defun mivi-copy-line (&optional arg)
  "Copy the current line.
With ARG, copy the specified number of lines relative to the current line."
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
  "Delete text covered by subsequent motion.
ARG is passed to the motion command."
  (interactive "P")
  (mivi--switch-state 'mivi-delete-state)
  (setq prefix-arg arg))

(defun mivi-delete-line (&optional arg)
  "Delete the current line.
With ARG, delete the specified number of lines relative to the current line."
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
  (back-to-indentation)
  (mivi--switch-state 'mivi-command-state)
  (mivi--store-command :command 'mivi-delete-line))

;; Scroll commands
(defun mivi-scroll-down (&optional arg)
  "Scroll down text by half of window height.
With ARG, scroll down by the specified lines."
  (interactive "P")
  (condition-case nil
      (scroll-down (mivi--numeric-or-default arg (/ (window-body-height) 2)))
    (error (goto-char (point-min)))))

(defun mivi-scroll-screen-down (&optional arg)
  "Scroll down text by window height.
With ARG, scroll down by the specified times."
  (interactive "p")
  (condition-case nil
      (scroll-down (* (window-body-height) (prefix-numeric-value arg)))
    (error (goto-char (point-min)))))

(defun mivi-scroll-screen-up (&optional arg)
  "Scroll up text by window height.
With ARG, scroll up by the specified times."
  (interactive "p")
  (condition-case nil
      (scroll-up (* (window-body-height) (prefix-numeric-value arg)))
    (error (goto-char (point-max)))))

(defun mivi-scroll-up (&optional arg)
  "Scroll up text by half of window height.
With ARG, scroll up by the specified lines."
  (interactive "P")
  (condition-case nil
      (scroll-up (mivi--numeric-or-default arg (/ (window-body-height) 2)))
    (error (goto-char (point-max)))))

;; Other commands
(defun mivi-backward-indent ()
  "Delete one indent shift before the current point.
Width of indent shift is configured by `mivi-shift-width'.
If the previous character is '0', delete all shifts till beginning of line.
If the previous character is '^', delete all shifts till the indent point."
  (interactive)
  (cond
   ((looking-back "^[[:blank:]]+\\([0^]\\)?" nil)
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
                            (- column mivi-shift-width)))))))
   ((not (eobp))
    (delete-char 1))))

(defun mivi-backward-kill-word ()
  "Kill word backward, using `mivi-backward-word'."
  (interactive)
  (let* ((beg (point))
         (end (progn
                (mivi-backward-word 1)
                (point))))
    (when (/= beg end)
      (kill-region beg end))))

(defun mivi-command ()
  "Enter command state and set cursor position.
It stores the inserted contents for repeat command."
  (interactive)
  (when (and (marker-position mivi--insert-beginning)
             (<= mivi--insert-beginning (point)))
    (if (marker-position mivi--insert-end)
        (let ((end (if (and (or (eq (plist-get mivi--last-command :command)
                                    'mivi-Replace)
                                (= mivi--insert-end mivi--insert-beginning))
                            (< mivi--insert-end (point)))
                       (point)
                     (1- mivi--insert-end))))
          (when (< mivi--insert-beginning end)
            (setq mivi--last-command
                  (plist-put mivi--last-command :content
                             (buffer-substring mivi--insert-beginning end)))))
      (setq mivi--last-command
            (plist-put mivi--last-command :content
                       (buffer-substring mivi--insert-beginning (point-max))))))
  (set-marker mivi--insert-beginning nil)
  (set-marker mivi--insert-end nil)

  (cond
   ((memq last-command '(mivi-open mivi-Open))
    (indent-to-left-margin))
   ((not (bolp))
    (backward-char)))
  (overwrite-mode -1)
  (mivi--switch-state 'mivi-command-state))

(defun mivi-join (&optional arg)
  "Join the next line after the current line.
Beginning spaces of the next line are deleted and one space is added between
lines.
With ARG, join the specified number of lines including the current line."
  (interactive "p")
  (mivi--store-command)
  (dotimes (_ (if (< arg 3) 1 (1- arg)))
    (if (looking-at-p "^$")
        (delete-char 1)
      (end-of-line)
      (unless (eobp)
        (let ((limit (point))
              (end-blank? (looking-back mivi--blank-regexp (1- (point)))))
          (forward-line 1)
          (let* ((end (save-excursion
                        (skip-chars-forward mivi--blank-chars)
                        (point)))
                 (beg (save-excursion
                        (skip-chars-backward mivi--blanknl-chars limit)
                        (point))))
            (delete-region beg end))
          (unless end-blank?
            (insert " "))
          (backward-char))))))

(defun mivi-kill-char (&optional arg)
  "Kill character at the current point.
With ARG, kill the specified number of characters."
  (interactive "p")
  (mivi--store-command)
  (kill-forward-chars arg))

(defun mivi-kill-backward-char (&optional arg)
  "Kill character before the current point.
With ARG, kill the specified number of backward characters."
  (interactive "p")
  (mivi--store-command)
  (kill-backward-chars arg))

(defun mivi-kill-region ()
  "Kill region or searched text.
If search overlay is active, it kills the overlay region.
Otherwise, call `kill-region' interactively."
  (interactive)
  (mivi--kill-with-function #'kill-region))

(defun mivi-kill-ring-save ()
  "Copy region or searched text to `kill-ring'.
If search overlay is active, it kills the overlay region.
Otherwise, call `kill-ring-save' interactively."
  (interactive)
  (mivi--kill-with-function #'kill-ring-save))

(defun mivi-mark (ch)
  "Mark the current point with character CH."
  (interactive "c")
  (mivi--put-mark ch))

(defun mivi-repeat (&optional arg)
  "Repeat the last executed mivi command.
ARG is passed to the repeated command."
  (interactive "P")
  (if (or (eq last-command 'mivi-undo)
          (and mivi--undo-repeating (eq last-command 'mivi-repeat)))
      (progn
        (if (eq mivi--undo-direction 'undo)
            (undo-tree-undo)
          (undo-tree-redo))
        (setq mivi--undo-repeating t))

    (setq mivi--undo-repeating nil)
    (let ((category (plist-get mivi--last-command :category))
          (command (plist-get mivi--last-command :command))
          (content (plist-get mivi--last-command :content))
          (prefix (plist-get mivi--last-command :prefix)))
      (cond
       ((memq command mivi--non-repeatable-commands)
        nil)
       ((eq category 'insert)
        (let ((count (mivi--numeric-or-default arg (or prefix 1)))
              (m (make-marker)))
          (when content
            (when (eq command 'mivi-Replace)
              (delete-region (point) (min (+ (length content) (point))
                                          (line-end-position))))
            (dotimes (_ count)
              (unless (eq command 'mivi-Replace)
                (funcall command t))
              (insert content)
              (when (or (not (marker-position m))
                        (< (marker-position m) (point)))
                (set-marker m (point))))
            (setq mivi--last-command
                  (plist-put mivi--last-command :prefix count))
            (goto-char (1- (marker-position m)))
            (set-marker m nil))))

       ((eq category 'change)
        (let ((this-command command)
              (current-prefix-arg (or arg prefix))
              (mivi--current-find-char mivi--last-find-char-for-repeat)
              (mivi--current-search-string (car mivi--last-search)))
          (when content
            (call-interactively command)
            (setq mivi--last-command
                  (plist-put mivi--last-command :content content))
            (insert content)
            (goto-char (1- (point)))
            (mivi--switch-state 'mivi-command-state))))

       (command
        (let ((this-command command)
              (current-prefix-arg (or arg prefix))
              (mivi--current-find-char mivi--last-find-char-for-repeat)
              (mivi--current-replace-char mivi--last-replace-char)
              (mivi--current-search-string (car mivi--last-search)))
          (call-interactively command)))))))

(defun mivi-repeat-subst ()
  "Substitute the last used regexp and replace in the current line."
  (interactive)
  (if mivi--last-subst
      (let ((region
             (save-excursion
               (cons
                (progn (forward-line 0) (point))
                (progn (forward-line 1) (point))))))
        (mivi--subst-internal
         (car mivi--last-subst)
         (cdr mivi--last-subst)
         (car region)
         (cdr region)
         nil))
    (user-error "No previous regular expression")))

(defun mivi-paste (&optional arg)
  "Paste the most recent killed text after the current point.
If the killed text is \"line\", it is pasted below the current line.
With ARG, paste the same text by the specified times."
  (interactive "p")
  (let ((line-paste (string-match-p "\n$" (current-kill 0))))
    (cond
     (line-paste
      (forward-line 1)
      (unless (bolp)
        (newline)))
     ((not (eolp))
      (forward-char)))
    (dotimes (_ arg)
      (save-excursion (insert (current-kill 0))))
    (when line-paste
      (back-to-indentation)))
  (mivi--store-command))

(defun mivi-Paste (&optional arg)
  "Paste the most recent killed text before the current point.
If the killed text is \"line\", it is pasted above the current line.
With ARG, paste the same text by the specified times."
  (interactive "p")
  (let ((line-paste (string-match-p "\n$" (current-kill 0))))
    (when line-paste
      (forward-line 0))
    (save-excursion
      (dotimes (_ arg)
        (insert (current-kill 0))))
    (when line-paste
      (back-to-indentation)))
  (mivi--store-command))

(defun mivi-replace-char (&optional arg)
  "Replace character at the current point with inputted one.
With ARG, replace the specified number of characters."
  (interactive "p")
  (let ((c (or mivi--current-replace-char (read-char "r-"))))
    (when (characterp c)
      (delete-char arg)
      (save-excursion
        (insert-char c arg)))
    (setq mivi--last-replace-char c)
    (mivi--store-command)))

;;
;; The following escape handling code
;; (`mivi--tty-escape-setup', `mivi--tty-escape-filter')
;; is mostly derived from viper.el.
;;
(defun mivi--tty-escape-setup ()
  "Setup tty ESC to work as `escape'.

Derived from `viper-catch-tty-ESC'."
  (let ((esc-binding (catch 'found
                       (map-keymap
                        (lambda (k b) (if (equal ?\e k) (throw 'found b)))
                        input-decode-map))))
    (define-key input-decode-map
      [?\e] `(menu-item "" ,esc-binding :filter ,#'mivi--tty-escape-filter))))

(defun mivi-undo ()
  "Undo or redo the last operatoin alternately.
If the last operation is undo, it redos the undone operation.
To perform multiple undo's, run `mivi-repeat' just after undo."
  (interactive)
  (if (eq mivi--undo-direction 'undo)
      (progn
        (undo-tree-redo)
        (setq mivi--undo-direction 'redo))
    (undo-tree-undo)
    (setq mivi--undo-direction 'undo)))

(defun mivi-updown-case (&optional arg)
  "Upcase or downcase character at the current point according to its case.
With ARG, upcase or downcase the specified number of characters."
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

;; Internal functions
(defun mivi--blankline-p (n)
  "Return non-nil if the line consists of blank characters only.
If N is not 0, it forwards the specified number of lines first."
  (save-excursion
    (forward-line n)
    (looking-at-p mivi--blankline-regexp)))

(defun mivi--find-internal (ch till? count)
  "Find character CH in the current line and move to the position.
If TILL? is non-nil, move one character before or after the position
according to sign of COUNT, positive or negative.
If COUNT is greater than 1, it finds COUNT'th occurrence of CH.
If COUNT is negative, it finds CH backward."
  (let ((case-fold-search nil)
        (sign (if (> count 0) 1 -1))
        (move? (and (> count 0) (not (eobp))))
        (limit (if (> count 0) (line-end-position) (line-beginning-position))))
    (when (/= (point) limit)
      (when move?
        (forward-char sign))
      (if (search-forward (char-to-string ch) limit t count)
          (when till?
            (forward-char (- sign)))
        (when mivi--unmatch-throw-error
          (when move?
            (forward-char (- sign)))
          (user-error "`%s' not found" (char-to-string ch))))
      (when move?
        (forward-char (- sign))))))

(defun mivi--kill-with-function (func)
  "Kill region or searched text with function FUNC.
If search overlay is active, it calls FUNC with the overlay region.
Otherwise, call FUNC interactively."
  (if mivi--search-overlay
      (let ((beg (overlay-start mivi--search-overlay))
            (end (overlay-end mivi--search-overlay)))
        (if (and beg end)
            (funcall func beg end)
          (call-interactively func)))
    (call-interactively func)))

(defun mivi--numeric-or-default (arg &optional default)
  "Return numeric value of prefix ARG or DEFAULT.
If arg is nil, then return DEFAULT.
If DEFAULT is also nil, return 0."
  (if (not arg)
      (or default 0)
    (prefix-numeric-value arg)))

(defun mivi--store-command (&rest args)
  "Store the current command for `mivi-repeat'.
The stored command is a plist with the following properties:

  :command	command symbol, default to `this-command'.
  :prefix	prefix argument, default to `current-prefix-arg'.
  :category	command category, 'insert, 'change, 'delete, default to nil.
  :content	inserted content, default to nil.

With ARGS, the above default values can be overwritten."
  (let ((plist (list :prefix current-prefix-arg
                     :command this-command)))
    (while args
      (pcase-let ((`(,prop ,val . ,rest) args))
        (setq plist (plist-put plist prop val))
        (setq args rest)))
    (setq mivi--last-command plist)))

(defun mivi--switch-state (state)
  "Switch MiVi state to STATE.
It also changes cursor type, sets markers in inserted region
and updates MiVi mode line."
  (let (new-cursor-type)
    (cond
     ((eq state 'mivi-replace-state)
      (setq new-cursor-type '(hbar . 7))
      (setq state 'mivi-insert-state))
     ((eq state 'mivi-insert-state)
      (setq new-cursor-type 'bar))
     (t
      (setq new-cursor-type (default-value 'cursor-type))))
    (setq cursor-type new-cursor-type))

  (when (memq state '(mivi-insert-state mivi-change-state))
    (set-marker mivi--insert-beginning (point))
    (if (eobp)
        (set-marker mivi--insert-end nil)
      (set-marker mivi--insert-end (1+ (point)))))

  (dolist (s mivi--states)
    (set s (eq s state)))
  (mivi--mode-line-update))

;; mode related
(defvar mivi-mode-map-alist
  (list
   (cons 'mivi-change-state mivi-change-map)
   (cons 'mivi-command-state mivi-command-map)
   (cons 'mivi-copy-state mivi-copy-map)
   (cons 'mivi-delete-state mivi-delete-map)
   (cons 'mivi-insert-state mivi-insert-map)))

(defvar-local mivi-mode-line nil)
(put 'mivi-mode-line 'risky-local-variable t)

(defun mivi--mode-line-update ()
  "Update MiVi mode line indicator according to the current sate."
  (setq mivi-mode-line
        `(mivi-mode
          (" "
           ,(cond
             (mivi-insert-state
              (propertize "[I]" 'face 'mivi-mode-line
                          'help-echo "MiVi: INSERT state"))
             (mivi-change-state
              (propertize "[C]" 'face 'mivi-mode-line
                          'help-echo "MiVi: CHANGE state"))
             (mivi-delete-state
              (propertize "[D]" 'face 'mivi-mode-line
                          'help-echo "MiVi: DELETE state"))
             (mivi-copy-state
              (propertize "[Y]" 'face 'mivi-mode-line
                          'help-echo "MiVi: COPY state"))
             (t
              (propertize "[-]" 'help-echo "MiVi: COMMAND state"))))))
  (force-mode-line-update))

(defun mivi--mode-line-insert ()
  "Insert MiVi mode line indicator after `mode-line-mule-info'."
  (when (listp mode-line-format)
    (setq mode-line-format
          (let (l)
            (dolist (e mode-line-format (nreverse l))
              (unless (eq e 'mivi-mode-line)
                (push e l))
              (when (eq e 'mode-line-mule-info)
                (push 'mivi-mode-line l)))))))

(defun mivi--mode-line-remove ()
  "Remove MiVi mode line indicator."
  (when (listp mode-line-format)
    (setq mode-line-format
          (delete 'mivi-mode-line mode-line-format))))

(define-minor-mode mivi-mode
  "Toggle MiVi mode in the current buffer."
  :init-value nil
  (if mivi-mode
      (progn
        (add-hook 'after-change-functions #'mivi--after-change-function nil t)
        (add-hook 'post-command-hook #'mivi--post-command-function nil t)
        (mivi--init-marks)
        (mivi--mode-line-insert)
        (unless (cl-some #'symbol-value mivi--states)
          (mivi--switch-state 'mivi-command-state))
        (unless undo-tree-mode
          (undo-tree-mode 1)))
    (remove-hook 'after-change-functions #'mivi--after-change-function t)
    (remove-hook 'post-command-hook #'mivi--post-command-function t)
    (mivi--mode-line-remove)
    (setq cursor-type (default-value 'cursor-type))
    (mapc #'kill-local-variable
          '(mivi-change-state
            mivi-command-state
            mivi-copy-state
            mivi-delete-state
            mivi-insert-state))))

(defun mivi--after-change-function (_beg _end _len)
  "Reset undo direction unless undo is in progress after change."
  (unless undo-in-progress
    (setq mivi--undo-direction 'redo)))

(defun mivi--post-command-function ()
  "Cleanup search overlay after non-search commands."
  (when (and mivi--search-overlay
             (not (memq this-command '(mivi-search
                                       mivi-search-backward
                                       mivi-search-current-word
                                       mivi-search-next
                                       mivi-search-Next))))
    (delete-overlay mivi--search-overlay)))

(defun mivi--tty-escape-filter (map)
  "Filter MAP to convert single ESC to `escape' in `mivi-mode'.
It returns MAP as is if it is in `mivi-command-state' or
`mivi-tty-escape-timeout' passes.

Derived from `viper--tty-ESC-filter'."
  (if (and mivi-mode
           (not mivi-command-state)
           (equal (this-single-command-keys) [?\e])
           (sit-for mivi-tty-escape-timeout))
      [escape]
    map))

(defun mivi-mode-on ()
  "Turn on mivi-mode in the current buffer if conditions are satisfied."
  (when (and (not (minibufferp))
             (not (eq (aref (buffer-name) 0) ?\s))
             (or (member major-mode mivi-enabled-major-modes)
                 (seq-some #'derived-mode-p mivi-enabled-derived-modes)))
    (mivi-mode 1)))

(defun mivi-mode-off ()
  "Turn off mivi-mode in the current buffer."
  (mivi-mode -1))

(define-globalized-minor-mode mivi-global-mode mivi-mode mivi-mode-on)

;;;###autoload
(defun mivi-setup ()
  "Setup mivi initial configurations and enable `mivi-global-mode'."

  (setq emulation-mode-map-alists
        (cons mivi-mode-map-alist emulation-mode-map-alists))

  (when mivi-override-universal-argument-map
    (define-key universal-argument-map (kbd "C-u") nil))

  (if (memq (terminal-live-p (frame-terminal)) '(t pc))
      (add-hook 'tty-setup-hook #'mivi--tty-escape-setup)
    (define-key input-decode-map [?\e] [escape]))

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
     "mivi-goto-mark"
     "mivi-goto-mark-line"
     "mivi-goto-pair"
     "mivi-next-line-at-bot"
     "mivi-next-paragraph"
     "mivi-next-sentence"
     "mivi-previous-line-at-bot"
     "mivi-previous-paragraph"
     "mivi-previous-sentence"
     "mivi-repeat-find"
     "mivi-repeat-find-opposite"
     "mivi-scroll-down"
     "mivi-scroll-screen-down"
     "mivi-scroll-screen-up"
     "mivi-scroll-up"
     "mivi-search"
     "mivi-search-Next"
     "mivi-search-backward"
     "mivi-search-current-word"
     "mivi-search-next"
     "mivi-window-bottom"
     "mivi-window-middle"
     "mivi-window-top"))

  (mivi-global-mode 1))

(provide 'mivi)
;;; mivi.el ends here
