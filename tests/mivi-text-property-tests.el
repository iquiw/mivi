(require 'ert)
(require 'mivi)

(ert-deftest mivi-copy-filters-text-properties ()
  (with-temp-buffer
    (insert "foo\n")
    (goto-char (point-min))
    (add-text-properties (point) (1+ (point)) '(line-prefix "   "))
    (let ((filter-buffer-substring-function
           (lambda (beg end _delete)
             (let ((s (buffer-substring beg end)))
               (remove-text-properties 0 1 '(line-prefix nil) s)
               s))))
      (mivi-copy-line 1))
    (mivi-paste 1)
    (should-not (get-text-property (point) 'line-prefix))))
