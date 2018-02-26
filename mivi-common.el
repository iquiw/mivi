;;; mivi-common.el --- MiVi common -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/mivi

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

;; This file provides variables and functions shared by mivi.el and
;; mivi-ex.el.

;;; Code:

(require 'outline)

(defvar mivi--last-search nil)
(defvar mivi--last-subst nil)
(defvar mivi--search-overlay nil)
(defvar-local mivi--mark-slots nil)

(defun mivi--init-marks ()
  "Initialize mark slots for the current buffer."
  (unless mivi--mark-slots
    (setq mivi--mark-slots (make-hash-table :test 'eq))))

(defun mivi--get-mark (ch)
  "Get mark for a character CH."
  (gethash ch mivi--mark-slots))

(defun mivi--put-mark (ch)
  "Put mark for a character CH at the current point."
  (puthash ch (point) mivi--mark-slots))

(defun mivi--copy-region (beg end &optional append-final-newline)
  "Copy region between BEG and END to `kill-ring'.
If APPEND-FINAL-NEWLINE is non-nil and END is end of buffer, append \"\\n\"."
  (kill-new (buffer-substring beg end))
  (when (and append-final-newline
             (eq end (point-max)))
    (kill-append "\n" nil)))

(defun mivi--goto-line (num)
  "Go to line NUM."
  (goto-char (point-min))
  (forward-line (1- num)))

(defsubst mivi--linepos-new (num pos)
  "Create new linepos data from line NUM and position POS.
At most one of argument can be nil."
  (cons num pos))

(defun mivi--linepos-line (lp &optional noresolve)
  "Return line number of linepos LP after resolving it if necessary.
If NORESOLVE is non-nil, it does not try resolving."
  (if (or noresolve (car lp))
      (car lp)
    (mivi--linepos-resolve lp)
    (car lp)))

(defun mivi--linepos-pos (lp &optional noresolve)
  "Return position of linepos LP after resolving it if necessary.
If NORESOLVE is non-nil, it does not try resolving."
  (if (or noresolve (cdr lp))
      (cdr lp)
    (mivi--linepos-resolve lp)
    (cdr lp)))

(defsubst mivi--linepos-add-line (lp num)
  "Return new linepos by adding line number of LP and NUM.
Line position of the return value is unresolved (nil)."
  (mivi--linepos-new (+ (mivi--linepos-line lp) num) nil))

(defun mivi--linepos-resolve (lp)
  "Resolve either line number or position of linepos LP if necessary."
  (cond
   ((null (car lp))
    (let ((pos (cdr lp)))
      (setcar lp (line-number-at-pos pos))))
   ((null (cdr lp))
    (let ((num (car lp)))
      (setcdr lp (save-excursion
                   (mivi--goto-line num)
                   (point)))))))

(defun mivi--search-internal (re count sign)
  "Search regexp RE in COUNT'th occurrence.
If SIGN should be 1 or -1, -1 means backward search."
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
          (push-mark origin t)
          (if mivi--search-overlay
              (move-overlay mivi--search-overlay
                            (match-beginning 0) (match-end 0)
                            (current-buffer))
            (setq mivi--search-overlay
                  (make-overlay (match-beginning 0) (match-end 0)))
            (overlay-put mivi--search-overlay 'face 'mivi-search-highlight))
          (goto-char (match-beginning 0))
          (mivi--show-hidden-text))
      (goto-char origin)
      (user-error "Pattern not found"))))

(defun mivi--show-hidden-text ()
  "Show hidden text in outline mode."
  (when (outline-invisible-p)
    (save-excursion
      (when (outline-previous-heading)
        (outline-show-entry)))))

(defun mivi--subst-internal (regexp replace beg end global)
  "Substitute REGEXP with REPLACE in region between BEG and END.
If GLOBAL is non-nil, it substitutes all occurrence in the region."
  (let* ((last-replace-point (point))
         (found t)
         (case-fold-search nil)
         (marker (set-marker (make-marker) end)))
    (goto-char beg)
    (while (and found (< (point) marker))
      (setq found (re-search-forward regexp
                                     (if global marker (line-end-position))
                                     t))
      (when found
        (replace-match replace t)
        (setq last-replace-point (save-excursion
                                   (forward-line 0)
                                   (point))))
      (unless global
        (forward-line 1)
        (setq found (not (eobp)))))
    (set-marker marker nil)
    (goto-char last-replace-point)))

(provide 'mivi-common)
;;; mivi-common.el ends here
