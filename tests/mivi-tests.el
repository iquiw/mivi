(require 'ert)
(require 'mivi-ex)

(ert-deftest mivi-ex--parse-linespec-for-number-only ()
  (should (equal '(1 . "") (mivi-ex--parse-linespec "1"))))

(ert-deftest mivi-ex--parse-linespec-for-number-and-rest ()
  (should (equal '(12 . ",34") (mivi-ex--parse-linespec "12,34"))))

(ert-deftest mivi-ex--parse-linespec-for-current-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 4)
    (should (equal '(4 . ",$") (mivi-ex--parse-linespec ".,$")))))

(ert-deftest mivi-ex--parse-linespec-for-last-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 1)
    (should (equal '(6 . "") (mivi-ex--parse-linespec "$")))))

(ert-deftest mivi-ex--parse-linespec-for-plus-number ()
  (should (equal '(114 . "") (mivi-ex--parse-linespec "99+15"))))

(ert-deftest mivi-ex--parse-linespec-for-number-plus ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (should (equal '(4 . ",.+5") (mivi-ex--parse-linespec ".+,.+5")))))

(ert-deftest mivi-ex--parse-linespec-for-plus-only ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (should (equal '(4 . ",+2") (mivi-ex--parse-linespec "+,+2")))))

(ert-deftest mivi-ex--parse-linespec-for-minus-number ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (should (equal '(3 . ",.") (mivi-ex--parse-linespec ".-3,.")))))

(ert-deftest mivi-ex--parse-linespec-for-number-minus ()
  (should (equal '(9 . ",10+") (mivi-ex--parse-linespec "10-,10+"))))

(ert-deftest mivi-ex--parse-linespec-for-minus-only ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 3)
    (should (equal '(2 . ",+") (mivi-ex--parse-linespec "-,+")))))

(ert-deftest mivi-ex--parse-linespec-for-no-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 4)
    (should (equal '(4 . "s/foo/bar/") (mivi-ex--parse-linespec "s/foo/bar/")))))

(ert-deftest mivi-ex--parse-linespec-for-unset-mark ()
  (with-temp-buffer
    (mivi-mode 1)
    (should-error (mivi-ex--parse-linespec "'a,.") :type 'user-error)))

(ert-deftest mivi-ex--parse-command-without-line-range ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (goto-line 2)
    (should (equal '(:command "s" :arg "/a/b/" :range (2 . 2))
                   (mivi-ex--parse-command "s/a/b/")))))

(ert-deftest mivi-ex--parse-command-with-one-line ()
  (should (equal '(:command "d" :arg "" :range (3 . 3))
                 (mivi-ex--parse-command "3d"))))

(ert-deftest mivi-ex--parse-command-with-number-range ()
  (should (equal '(:command "t" :arg "$" :range (10 . 123))
                 (mivi-ex--parse-command "10,123t$"))))

(ert-deftest mivi-ex--parse-command-of-multiple-chars ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (should (equal '(:command "foo" :arg "$" :range (5 . 6))
                   (mivi-ex--parse-command "-,.foo$")))))

(ert-deftest mivi-ex--parse-command-with-spaces-skipped ()
  (with-temp-buffer
    (insert "1\n2\n3\n4\n5\n")
    (should (equal '(:command "d" :arg "3" :range (6 . 6))
                   (mivi-ex--parse-command "d 3")))))
