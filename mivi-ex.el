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
(defun mivi-ex (arg)
  "Parse ARG as ex command and dispatch it to other ex functions.
When called interactively, ex command is read from user input."
  (interactive (let ((default (car mivi-ex--history)))
                 (list (read-string (if default
                                        (format ":(default %s) " default)
                                      ":")
                                    nil 'mivi-ex--history default))))
  (let* ((cmdspec (mivi-ex--parse-command arg))
         (command (plist-get cmdspec :command)))
    (if (null command)
        (goto-char (mivi--linepos-pos (cdr (plist-get cmdspec :range))))
      (let ((region (mivi-ex--range-to-region (plist-get cmdspec :range))))
        (mivi-ex--dispatch command region (plist-get cmdspec :arg))))))

(defun mivi-ex--dispatch (command region &optional arg)
  "Dispatch ex COMMAND to run in REGION with optional ARG."
  (pcase command
    ("d" (mivi-ex--delete region))
    ("g" (mivi-ex--global region arg))
    ("s" (mivi-ex--subst region arg))
    ("v" (mivi-ex--global region arg t))
    ("y" (mivi-ex--copy region))))

(defun mivi-ex--copy (region)
  "Copy lines within REGION."
  (mivi--copy-region (car region) (cdr region)))

(defun mivi-ex--delete (region)
  "Delete lines within REGION."
  (kill-region (car region) (cdr region))
  (when (eobp)
    (forward-line -1)))

(defun mivi-ex--global (region arg &optional inverse)
  "Dispatch ex command for matched lines in REGION.
Ex command is provided by ARG.
If INVERSE is non-nil, it processes unmatched lines instead."
  (let* ((beg (car region))
         (end (cdr region))
         (subspec (mivi-ex--parse-subst arg t))
         (regexp (plist-get subspec :regexp))
         (rest (plist-get subspec :rest)))
    (unless (or (string= rest "")
                (string-match-p "\\`[gv]" rest))
      (let ((command (substring rest 0 1))
            (arg (substring rest 1))
            (end-marker (set-marker (make-marker) end))
            (next-marker (make-marker)))
        (setq mivi--last-search (cons regexp 1))
        (goto-char beg)
        (let ((line-beg (point))
              (line-end (save-excursion (forward-line 1) (point)))
              done)
          (while (and (not done) (not (eobp)))
            (let* ((bound (if inverse line-end end-marker))
                   (matched (re-search-forward regexp bound t)))
              (cond
               ((and (not inverse) (not matched))
                (setq done t))

               ((or (and (not inverse) matched)
                    (and inverse (not matched)))
                (setq line-beg (progn (forward-line 0) (point)))
                (setq line-end (progn (forward-line 1) (point)))
                (set-marker next-marker line-end)
                (mivi-ex--dispatch command (cons line-beg line-end) arg)
                (goto-char next-marker))

               (t
                (forward-line 1)
                (setq line-beg (progn (forward-line 0) (point)))
                (setq line-end (progn (forward-line 1) (point))))))))
        (set-marker next-marker nil)
        (set-marker end-marker nil)))))

(defun mivi-ex--subst (region arg)
  "Substitute lines within REGION according to ARG."
  (let* ((beg (car region))
         (end (cdr region))
         (subspec (mivi-ex--parse-subst arg))
         (regexp (plist-get subspec :regexp))
         (replace (plist-get subspec :replace))
         (options (plist-get subspec :options))
         (global (memq 'global options)))
    (when (string= regexp "")
      (if mivi--last-search
          (setq regexp (car mivi--last-search))
        (user-error "No previous regular expression")))
    (mivi--subst-internal regexp replace beg end global)
    (setq mivi--last-subst (cons regexp replace))))

;; Internal functions
(defun mivi-ex--parse-command (str)
  "Parse ex command line provided as STR.
It returns plist of :command, :arg and :range."
  (let (beg end)
    (pcase (mivi-ex--parse-linespec str)
      (`(,lp . ,rest)
       (setq beg lp)
       (setq str rest)))
    (when (string-match-p "\\`," str)
      (pcase (mivi-ex--parse-linespec (substring str 1))
        (`(,lp . ,rest)
         (setq end lp)
         (setq str rest))))
    (cond
     ;; whole lines
     ((null beg)
      (setq beg (mivi--linepos-new nil (point-min)))
      (setq end (mivi--linepos-new nil (point-max))))

     ;; current line
     ((null end) (setq end beg))

     ((> (mivi--linepos-line beg) (mivi--linepos-line end))
      (user-error "The second address is smaller than the first")))

    (if (string-match "\\`\\([a-z]+\\)[[:blank:]]*" str)
        (list :command (match-string 1 str)
              :arg (substring str (match-end 0))
              :range (cons beg end))
      (list :range (cons beg end)))))

(defun mivi-ex--parse-linespec (str)
  "Parse ex line number spec provided as STR.
It returns cons of line position and rest of string.
Line position nil means the whole lines."
  (let (lp)
    (cond
     ;; whole lines
     ((string-match "\\`%" str)
      (setq str (substring str 1)))

     ;; line number
     ((string-match "\\`[0-9]+" str)
      (setq lp (mivi--linepos-new (string-to-number (match-string 0 str)) nil))
      (setq str (substring str (match-end 0))))

     ;; current line
     ((string-match-p "\\`\\." str)
      (setq lp (mivi--linepos-new (line-number-at-pos)
                                  (save-excursion (forward-line 0) (point))))
      (setq str (substring str 1)))

     ;; marked line
     ((string-match "\\`'\\(.\\)" str)
      (let* ((c (string-to-char (match-string 1 str)))
             (p (mivi--get-mark c)))
        (setq str (substring str 2))
        (if p
            (save-excursion
              (goto-char p)
              (forward-line 0)
              (setq lp (mivi--linepos-new nil (point))))
          (user-error "`%s': Marker is not set" c))))

     ;; last line
     ((string-match-p "\\`\\$" str)
      (setq lp (mivi--linepos-new nil (save-excursion
                                        (goto-char (point-max))
                                        (forward-line 0)
                                        (point))))
      (setq str (substring str 1)))

     ;; search pattern
     ((string-match "\\`[/?]" str)
      (let* ((delim (match-string 0 str))
             (regexp
              (if (string-match delim str 1)
                  (prog1
                      (substring str 1 (match-beginning 0))
                    (setq str (substring str (match-end 0))))
                (prog1
                    (substring str 1)
                  (setq str "")))))
        (save-excursion
          (let ((sign (if (string= delim "/") 1 -1)))
            (mivi--search-internal regexp 1 sign)
            (setq mivi--last-search (cons regexp sign)))
          (forward-line 0)
          (setq lp (mivi--linepos-new nil (point))))))

     ((not (string-match-p "\\`[gv]" str))
      (setq lp (mivi--linepos-new (line-number-at-pos)
                                  (save-excursion (forward-line 0) (point))))))

    (when lp
      (when (string-match "\\`\\([-+]\\)\\([0-9]+\\)?" str)
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
          (user-error "Illegal address: only %s lines in the file" last-num))))
    (cons lp str)))

(defun mivi-ex--parse-subst (str &optional no-replace)
  "Parse argument of substitute command provided by STR.
It returns plist of :regexp, :replace, :rest and :options.
If NO-REPLACE is non-nil, it returns :rest instead of :replace and :options."
  (let* ((delim (substring str 0 1))
         (re-unesc (concat "\\\\" delim))
         offset1 offset2 regexp (replace "") (rest "") options)
    (if (setq offset1 (mivi-ex--find-subst-delimiter str delim 1))
        (progn
          (setq regexp (substring str 1 (1- offset1)))
          (setq rest (substring str offset1)))
      (setq regexp (substring str 1)))
    (when (and (not no-replace) offset1)
      (if (setq offset2 (mivi-ex--find-subst-delimiter str delim offset1))
          (let ((flags (substring str offset2)))
            (setq replace (substring str offset1 (1- offset2)))
            (cond
             ((string= flags "") nil)
             ((string= flags "g") (push 'global options))
             (t (user-error (format "`%s': Unknown flags" flags)))))
        (setq replace rest))
      (setq rest ""))
    (if no-replace
        (list :regexp (replace-regexp-in-string re-unesc delim regexp)
              :rest rest)
      (list :regexp (replace-regexp-in-string re-unesc delim regexp)
            :replace (replace-regexp-in-string re-unesc delim replace)
            :options options))))

(defun mivi-ex--find-subst-delimiter (str delim offset)
  "Find next delimiter in STR and returns the position.
Delimiter is specified by DELIM string, which should be one character.
Delimiter position is searched from OFFSET in STR.
It returns nil if not found."
  (let ((re-sep (concat "\\(\\\\*\\)" delim)))
    (catch 'index
      (while (string-match re-sep str offset)
        (let ((esc-len (length (match-string 1 str))))
          (if (= (% esc-len 2) 0)
              (throw 'index (match-end 0))
              ;(throw 'index (+ (match-beginning 0) esc-len))
            (setq offset (match-end 0))))))))

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
