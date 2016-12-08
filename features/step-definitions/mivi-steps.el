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
    (dotimes (i (* (window-body-height) (string-to-number pages)))
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

(Then "^the current top line should be half page down from beginning$"
  "Checks that the current line is half page scrolled down from beginning of buffer."
  (lambda ()
    (move-to-window-line 0)
    (should (= (+ (/ (window-body-height) 2) 1) (line-number-at-pos)))))

(Then "^the current bottom line should be half page up from end$"
  "Checks that the current line is half page scrolled down from end of buffer."
  (lambda ()
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
      (move-to-window-line -1)
      (should (= (- last-line (/ (window-body-height) 2)) (line-number-at-pos))))))

(Then "^the current top line should be \"\\(.+\\)\" pages? down from beginning$"
  "Checks that the current line is N pages scrolled down from beginning of buffer."
  (lambda (n)
    (move-to-window-line 0)
    (should (= (1+ (* (window-body-height) (string-to-number n))) (line-number-at-pos)))))

(Then "^the current bottom line should be \"\\(.+\\)\" pages? up from end$"
  "Checks that the current line is N pages scrolled up from end of buffer."
  (lambda (n)
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
      (move-to-window-line -1)
      (should (= (- last-line (* (window-body-height) (string-to-number n))) (line-number-at-pos))))))

(Then "^the current top line should be \"\\(.+\\)\" down from beginning$"
  "Checks that the current line is scrolled down LINE from beginning of buffer."
  (lambda (line)
    (move-to-window-line 0)
    (should (= (1+ (string-to-number line)) (line-number-at-pos)))))

(Then "^the current bottom line should be \"\\(.+\\)\" up from end$"
  "Checks that the current line is scrolled up LINE from end of buffer."
  (lambda (line)
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (line-number-at-pos))))
      (move-to-window-line -1)
      (should (= (- last-line (string-to-number line)) (line-number-at-pos))))))

(Then "^the current line should be \"\\(.+\\)\"$"
  "Checks that the current line number is LINE."
  (lambda (line)
    (should (= (string-to-number line) (line-number-at-pos)))))

(Then "^the current line should be in the middle of window$"
  "Checks that the current line is in the middle of window."
  (lambda ()
    (let ((first-line (save-excursion
                        (move-to-window-line 0)
                        (line-number-at-pos))))
      (should (= (+ first-line (/ (window-body-height) 2)) (line-number-at-pos))))))

(Then "^the current line should be the last line$"
  "Checks that the current line is the last line."
  (lambda ()
    (let ((last-line (save-excursion
                       (goto-char (point-max))
                       (1- (line-number-at-pos)))))
      (should (= last-line (line-number-at-pos))))))

(Then "^the mivi state should be \"\\(.+\\)\"$"
  "Checks that the mivi state is STATE."
  (lambda (state)
    (should (eq (symbol-value (intern (concat "mivi-" state "-state"))) t))))

(Then "^the current kill-ring should be\\(?: \"\\(.*\\)\"\\|:\\)$"
  "Checks that the current kill-ring is STRING."
  (lambda (string)
    (should (string= (current-kill 0) string))))
