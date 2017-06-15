(require 'ert)
(require 'mivi-ex)

(defun mivi-ex--test-linespec (num arg str)
  (let ((result (mivi-ex--parse-linespec str)))
    (should (equal num (mivi--linepos-line (car result))))
    (should (equal arg (cdr result)))))

(ert-deftest mivi-ex--parse-linespec-for-number-only ()
  (mivi-ex--test-linespec 1 "" "1"))

(ert-deftest mivi-ex--parse-linespec-for-number-and-rest ()
  (mivi-ex--test-linespec 12 ",34" "12,34"))

(ert-deftest mivi-ex--parse-linespec-for-current-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 4)
    (mivi-ex--test-linespec 4 ",$" ".,$")))

(ert-deftest mivi-ex--parse-linespec-for-last-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 1)
    (mivi-ex--test-linespec 6 "" "$")))

(ert-deftest mivi-ex--parse-linespec-for-plus-number ()
  (mivi-ex--test-linespec 114 "" "99+15"))

(ert-deftest mivi-ex--parse-linespec-for-number-plus ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (mivi-ex--test-linespec 4 ",.+5" ".+,.+5")))

(ert-deftest mivi-ex--parse-linespec-for-plus-only ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (mivi-ex--test-linespec 4 ",+2" "+,+2")))

(ert-deftest mivi-ex--parse-linespec-for-minus-number ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (mivi-ex--test-linespec 3 ",." ".-3,.")))

(ert-deftest mivi-ex--parse-linespec-for-number-minus ()
  (mivi-ex--test-linespec 9 ",10+" "10-,10+"))

(ert-deftest mivi-ex--parse-linespec-for-minus-only ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (mivi-ex--test-linespec 2 ",+" "-,+")))

(ert-deftest mivi-ex--parse-linespec-for-no-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 4)
    (mivi-ex--test-linespec 4 "s/foo/bar/" "s/foo/bar/")))

(ert-deftest mivi-ex--parse-linespec-for-unset-mark ()
  (with-temp-buffer
    (mivi-mode 1)
    (should-error (mivi-ex--parse-linespec "'a,.") :type 'user-error)))

(defun mivi-ex--test-command (command arg line-range str)
  (let* ((result (mivi-ex--parse-command str))
         (range (plist-get result :range)))
    (should (equal command (plist-get result :command)))
    (should (equal arg (plist-get result :arg)))
    (should (equal (car line-range) (mivi--linepos-line (car range))))
    (should (equal (cdr line-range) (mivi--linepos-line (cdr range))))))

(ert-deftest mivi-ex--parse-command-without-line-range ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 2)
    (mivi-ex--test-command "s" "/a/b/" '(2 . 2) "s/a/b/")))

(ert-deftest mivi-ex--parse-command-with-one-line ()
  (mivi-ex--test-command "d" "" '(3 . 3) "3d"))

(ert-deftest mivi-ex--parse-command-with-number-range ()
  (mivi-ex--test-command "t" "$" '(10 . 123) "10,123t$"))

(ert-deftest mivi-ex--parse-command-of-multiple-chars ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (mivi-ex--test-command "foo" "$" '(5 . 6) "-,.foo$")))

(ert-deftest mivi-ex--parse-command-with-spaces-skipped ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (mivi-ex--test-command "d" "3" '(6 . 6) "d 3")))

(ert-deftest mivi-ex--parse-command-with-empty-command ()
  (mivi-ex--test-command nil nil '(12 . 24) "12,24"))

(ert-deftest mivi-ex--parse-subst-with-1delim ()
  (should (equal '(:regexp "foo" :replace "")
                 (mivi-ex--parse-subst "/foo"))))

(ert-deftest mivi-ex--parse-subst-with-2delim ()
  (should (equal '(:regexp "foo" :replace "bar")
                 (mivi-ex--parse-subst "/foo/bar"))))

(ert-deftest mivi-ex--parse-subst-with-3delim ()
  (should (equal '(:regexp "foo" :replace "bar")
                 (mivi-ex--parse-subst "/foo/bar/"))))

(ert-deftest mivi-ex--parse-subst-with-comma-delim ()
  (should (equal '(:regexp "foo/bar" :replace "baz/")
                 (mivi-ex--parse-subst ",foo/bar,baz/,"))))

(ert-deftest mivi-ex--parse-subst-with-escape ()
  (should (equal '(:regexp "foo/bar/baz" :replace "qux")
                 (mivi-ex--parse-subst "/foo\\/bar\\/baz/qux/"))))
