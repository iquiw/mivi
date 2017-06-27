;;; mivi-ex.el --- MiVi ex commands -*- lexical-binding: t -*-

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

;; `mivi-ex' provides ex commands for `mivi-mode'.

;; It defines limited commands only in design.

;;; Code:

(require 'mivi-common)

(defvar mivi-ex--history nil)

;; ex commands
(defun mivi-ex ()
  "Read ex command from user input and dispatch it to other ex functions."
  (interactive)
  (let* ((default (car mivi-ex--history))
         (str (read-string (if default (format ":(default %s) " default) ":")
                           nil 'mivi-ex--history default))
         (cmdspec (mivi-ex--parse-command str)))
    (pcase (plist-get cmdspec :command)
      ('nil (goto-char (mivi--linepos-pos (cdr (plist-get cmdspec :range)))))
      ("d" (mivi-ex--delete (plist-get cmdspec :range)))
      ("s" (mivi-ex--subst (plist-get cmdspec :range)
                           (plist-get cmdspec :arg))))))

(defun mivi-ex--delete (range)
  "Delete lines within RANGE."
  (let ((region (mivi-ex--range-to-region range)))
    (kill-region (car region) (cdr region))))

(defun mivi-ex--subst (range arg)
  "Substitute lines within RANGE according to ARG."
  (let* ((region (mivi-ex--range-to-region range))
         (beg (car region))
         (end (cdr region))
         (subspec (mivi-ex--parse-subst arg))
         (regexp (plist-get subspec :regexp))
         (replace (plist-get subspec :replace))
         (options (plist-get subspec :options))
         (global (memq 'global options))
         (last-replace-point (point))
         (found t)
         (case-fold-search nil))
    (goto-char beg)
    (while (and found (< (point) end))
      (setq found (re-search-forward regexp
                                     (if global end (line-end-position))
                                     t))
      (when found
        (delete-region (match-beginning 0) (match-end 0))
        (insert replace)
        (setq last-replace-point (save-excursion
                                   (forward-line 0)
                                   (point))))
      (unless global
        (forward-line 1)
        (setq found (not (eobp)))))
    (goto-char last-replace-point)))

;; Internal functions
(defun mivi-ex--parse-command (str)
  "Parse ex command line provided as STR.
It returns plist of :command, :arg and :range."
  (let (beg end)
    (when (string-match-p "^%" str)
      (setq str (concat "1,$" (substring str 1))))
    (pcase (mivi-ex--parse-linespec str)
      (`(,lp . ,rest)
       (setq beg lp)
       (setq str rest)))
    (when (string-match-p "^," str)
      (pcase (mivi-ex--parse-linespec (substring str 1))
        (`(,lp . ,rest)
         (setq end lp)
         (setq str rest))))
    (cond
     ((null end) (setq end beg))
     ((> (mivi--linepos-line beg) (mivi--linepos-line end))
      (user-error "The second address is smaller than the first.")))
    (if (string-match "\\([a-z]+\\) *\\(.*\\)" str)
        (list :command (match-string 1 str)
              :arg (match-string 2 str)
              :range (cons beg end))
      (list :range (cons beg end)))))

(defun mivi-ex--parse-linespec (str)
  "Parse ex line number spec provided as STR.
It returns cons of line-position and rest of string."
  (let (lp)
    (cond
     ((string-match "^[0-9]+" str)
      (setq lp (mivi--linepos-new (string-to-number (match-string 0 str)) nil))
      (setq str (substring str (match-end 0))))
     ((string-match-p "^\\." str)
      (setq lp (mivi--linepos-new (line-number-at-pos)
                                  (save-excursion (forward-line 0) (point))))
      (setq str (substring str 1)))
     ((string-match "^'\\(.\\)" str)
      (let* ((c (string-to-char (match-string 1 str)))
             (p (mivi--get-mark c)))
        (setq str (substring str 2))
        (if p
            (setq lp (mivi--linepos-new nil p))
          (user-error "`%s': Marker is not set." c))))
     ((string-match-p "^\\$" str)
      (setq lp (mivi--linepos-new nil (save-excursion
                                        (goto-char (point-max))
                                        (forward-line 0)
                                        (point))))
      (setq str (substring str 1)))
     (t (setq lp (mivi--linepos-new (line-number-at-pos)
                                    (save-excursion (forward-line 0) (point))))))

    (when (string-match "^\\([-+]\\)\\([0-9]+\\)?" str)
      (let ((num (if (match-string 2 str)
                    (string-to-number (match-string 2 str))
                   1)))
        (setq lp (mivi--linepos-add-line lp (if (equal (match-string 1 str) "-")
                                                (- num)
                                              num))))
      (setq str (substring str (match-end 0))))
    (let* ((num (mivi--linepos-line lp t))
           (last-num (and num (line-number-at-pos (point-max)))))
      (when (and num (> num last-num))
        (user-error "Illegal address: only %s lines in the file." last-num)))
    (cons lp str)))

(defun mivi-ex--parse-subst (str)
  "Parse argument of substitute command provided by STR.
It returns plist of :regexp, :replace and options."
  (let* ((delim (substring str 0 1))
         (re-sep (concat "[^\\]" delim))
         (re-unesc (concat "\\\\" delim))
         offset regexp (replace "") options)
    (if (string-match re-sep str)
        (progn
          (setq regexp (substring str 1 (1+ (match-beginning 0))))
          (setq offset (match-end 0)))
      (setq regexp (substring str 1)))
    (when offset
      (if (string-match re-sep str (1- offset))
          (let ((flags (substring str (match-end 0))))
            (setq replace (substring str offset (1+ (match-beginning 0))))
            (cond
             ((string= flags "") nil)
             ((string= flags "g") (push 'global options))
             (t (user-error (format "`%s': Unknown flags" flags)))))
        (setq replace (substring str offset))))
    (list :regexp (replace-regexp-in-string re-unesc delim regexp)
          :replace (replace-regexp-in-string re-unesc delim replace)
          :options options)))

(defun mivi-ex--range-to-region (range)
  "Convert line RANGE to region, which is cons of points."
  (let ((beg (save-excursion
               (goto-char (mivi--linepos-pos (car range)))
               (point)))
        (end (save-excursion
               (goto-char (mivi--linepos-pos (cdr range)))
               (forward-line 1)
               (point))))
    (cons beg end)))

(provide 'mivi-ex)
;;; mivi-ex.el ends here
