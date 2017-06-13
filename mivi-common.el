;;; mivi-common.el --- MiVi common -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Iku Iwasa

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

(provide 'mivi-common)
;;; mivi-common.el ends here
