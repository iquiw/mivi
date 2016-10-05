;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I go to cell (\\([0-9]+\\), *\\([0-9]+\\))$"
  "Go to a specific (line, column)."
  (lambda (line column)
    (goto-char (point-min))
    (forward-line (- (string-to-number line) 1))
    (move-to-column (string-to-number column))))

(Then "^the cursor should be at cell (\\([0-9]+\\), *\\([0-9]+\\))$"
  "Checks that the cursor is at a specific (line, column)."
  (lambda (line column)
    (let ((message "Expected cursor to be at cell '(%s, %s)', but was at '(%s, %s)'"))
      (cl-assert (and (= (string-to-number line) (line-number-at-pos))
                      (= (string-to-number column) (current-column)))
                 nil
                 message
                 line column (line-number-at-pos) (current-column)))))
