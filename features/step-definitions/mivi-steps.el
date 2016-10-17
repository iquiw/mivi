;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I go to cell (\\([0-9]+\\), *\\([0-9]+\\))$"
  "Go to the specified (LINE, COLUMN)."
  (lambda (line column)
    (goto-char (point-min))
    (forward-line (- (string-to-number line) 1))
    (move-to-column (string-to-number column))))

(When "^I insert \"\\(.+\\)\" pages$"
  "Insert the specified PAGES."
  (lambda (pages)
    (dotimes (i (* (window-height) (string-to-number pages)))
      (insert (format "%s\n" i)))))

(When "^I recenter on line \"\\(.+\\)\"$"
  "Recenter to the specified LINE."
  (lambda (line)
    (recenter (string-to-number line))))

(Then "^the cursor should be at cell (\\([0-9]+\\), *\\([0-9]+\\))$"
  "Checks that the cursor is at a specific (LINE, COLUMN)."
  (lambda (line column)
    (should (equal (cons (string-to-number line) (string-to-number column))
                   (cons (line-number-at-pos) (current-column))))))

(Then "^the current line should be half page down from beginning$"
  "Checks that the current line is half page scrolled down from beginning of buffer."
  (lambda ()
    (move-to-window-line 0)
    (should (= (+ (/ (window-height) 2) 1) (line-number-at-pos)))))

(Then "^the current line should be half page up from end$"
  "Checks that the current line is half page scrolled down from end of buffer."
  (lambda ()
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
      (move-to-window-line -1)
      (should (= (- last-line (/ (window-height) 2)) (line-number-at-pos))))))

(Then "^the current line should be \"\\(.+\\)\" down from beginning$"
  "Checks that the current line is scrolled down LINE from beginning of buffer."
  (lambda (line)
    (move-to-window-line 0)
    (should (= (string-to-number line) (line-number-at-pos)))))

(Then "^the current line should be \"\\(.+\\)\" up from end$"
  "Checks that the current line is scrolled up LINE from end of buffer."
  (lambda (line)
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
      (move-to-window-line -1)
      (should (= (- last-line (string-to-number line)) (line-number-at-pos))))))
