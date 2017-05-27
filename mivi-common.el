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

(provide 'mivi-common)
;;; mivi-common.el ends here
