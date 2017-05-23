;;; mivi-ex.el --- MiVi common -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local mivi--mark-slots (make-hash-table :test 'eq))

(defun mivi--get-mark (ch)
  "Get mark for a character CH."
  (gethash ch mivi--mark-slots))

(defun mivi--put-mark (ch)
  "Put mark for a character CH at the current point."
  (puthash ch (point) mivi--mark-slots))

(provide 'mivi-common)
