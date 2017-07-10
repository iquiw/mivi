(require 'ert)
(require 'mivi)

(ert-deftest mivi-repeat-subst-throws-error-if-no-last-regexp ()
  (let ((mivi--last-subst nil))
    (should-error (execute-kbd-macro "&") :type 'user-error)))

(ert-deftest mivi-search-throws-error-if-no-match-found ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-char (point-min))
    (should-error (execute-kbd-macro "/foo") :type 'user-error)))

(ert-deftest mivi-search-backward-throws-error-if-no-match-found ()
  (with-temp-buffer
    (insert "foo\nbar\nbaz\n")
    (should-error (execute-kbd-macro "?FOO") :type 'user-error)))
