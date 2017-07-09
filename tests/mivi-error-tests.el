(require 'ert)
(require 'mivi)

(ert-deftest mivi-repeat-subst-throws-error-by-no-last-regexp ()
  (let ((mivi--last-subst nil))
    (should-error (mivi-repeat-subst) :type 'user-error)))
