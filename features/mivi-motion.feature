Feature: Motion
  In order to edit text in command mode
  As a user
  I want to move cursor

  Scenario: h
    Given the buffer is empty
    When I insert:
    """
    1234567890 1234 5678
    abcdefghij

    foo bar baz
    """
    And I go to point "3"
    And I type "h"
    Then the cursor should be at cell (1, 1)
    When I type "h"
    Then the cursor should be at cell (1, 0)
    When I go to line "3"
    When I type "h"
    Then the cursor should be at cell (2, 10)

  Scenario: C-h
    When I go to point "5"
    And I press "C-h"
    Then the cursor should be at cell (1, 3)
    When I start an action chain
    And I press "3"
    And I press "C-h"
    And I execute the action chain
    Then the cursor should be at cell (1, 0)

  Scenario: j
    When I go to beginning of buffer
    And I type "j"
    Then the cursor should be at cell (2, 0)
    When I go to end of line
    And I type "j"
    Then the cursor should be at cell (3, 0)
    When I go to cell (2, 10)
    And I type "jj"
    Then the cursor should be at cell (4, 10)

  Scenario: k
    When I go to end of buffer
    And I type "k"
    Then the cursor should be at cell (3, 0)
    When I go to cell (2, 5)
    And I type "k"
    Then the cursor should be at cell (1, 5)
    When I go to cell (4, 7)
    And I type "kk"
    Then the cursor should be at cell (2, 7)

  Scenario: l
    When I go to beginning of buffer
    And I type "l"
    Then the cursor should be at cell (1, 1)
    When I go to cell (2, 9)
    And I type "l"
    Then the cursor should be at cell (2, 10)
    When I type "l"
    Then the cursor should be at cell (3, 0)

  Scenario: backward word
    Given the buffer is empty
    When I insert:
    """
    abc def  ghi

      012---345-678 (abc)

    foo_bar<baz>
      ()
    """
    And I go to end of buffer
    And I type "b"
    Then the cursor should be at cell (6, 2)
    When I type "b"
    Then the cursor should be at cell (5, 11)
    When I type "b"
    Then the cursor should be at cell (5, 8)
    When I type "b"
    Then the cursor should be at cell (5, 7)
    When I type "3b"
    Then the cursor should be at cell (3, 17)
    When I go to cell (1, 10)
    And I type "b"
    Then the cursor should be at cell (1, 9)
    When I type "2b"
    Then the cursor should be at cell (1, 0)

  Scenario: end of word
    When I go to beginning of buffer
    And I type "e"
    Then the cursor should be at cell (1, 2)
    When I type "2e"
    Then the cursor should be at cell (1, 11)
    When I type "e"
    Then the cursor should be at cell (3, 4)
    When I type "10e"
    Then the cursor should be at cell (5, 10)
    When I type "e"
    Then the cursor should be at cell (5, 11)

  Scenario: forward word
    When I go to beginning of buffer
    And I type "w"
    Then the cursor should be at cell (1, 4)
    When I type "w"
    Then the cursor should be at cell (1, 9)
    When I type "w"
    Then the cursor should be at cell (3, 2)
    When I type "3w"
    Then the cursor should be at cell (3, 11)
    When I type "6w"
    Then the cursor should be at cell (5, 7)
    When I place the cursor after "678"
    And I type "w"
    Then the cursor should be at cell (3, 16)
    When I place the cursor after "<baz"
    And I type "w"
    Then the cursor should be at cell (6, 2)

  Scenario: Backward word
    When I go to end of buffer
    And I type "B"
    Then the cursor should be at cell (6, 2)
    When I type "B"
    Then the cursor should be at cell (5, 0)
    When I type "B"
    Then the cursor should be at cell (3, 16)
    When I type "3B"
    Then the cursor should be at cell (1, 4)

  Scenario: End of word
    When I go to beginning of buffer
    And I type "3E"
    Then the cursor should be at cell (1, 11)
    When I type "E"
    Then the cursor should be at cell (3, 14)
    When I type "E"
    Then the cursor should be at cell (3, 20)

  Scenario: forward Word
    When I go to beginning of buffer
    And I type "W"
    Then the cursor should be at cell (1, 4)
    When I type "3W"
    Then the cursor should be at cell (3, 16)
    When I type "W"
    Then the cursor should be at cell (5, 0)

  Scenario: find
    Given the buffer is empty
    When I insert:
    """
    abc def ghi

    012 345 678
    foo!bar!baz
    """
    And I go to beginning of buffer
    And I type "fh"
    Then the cursor should be at cell (1, 9)
    When I type "f2"
    Then the cursor should be at cell (3, 2)
    When I type "2f "
    Then the cursor should be at cell (3, 7)
    When I type "fofo"
    Then the cursor should be at cell (4, 2)
    When I type "fA"
    Then the cursor should be at cell (4, 2)
    When I go to end of buffer
    And I type "f@"
    Then the cursor should be at cell (4, 11)

  Scenario: Find
    When I go to end of buffer
    And I type "Fr"
    Then the cursor should be at cell (4, 6)
    When I type "Fi"
    Then the cursor should be at cell (1, 10)
    When I go to end of buffer
    And I type "FaFa"
    Then the cursor should be at cell (4, 5)
    When I go to end of buffer
    And I type "2Fa"
    Then the cursor should be at cell (4, 5)
    When I go to beginning of buffer
    When I type "F@"
    Then the cursor should be at cell (1, 0)

  Scenario: repeat find
    When I go to beginning of buffer
    And I type "f "
    And I type ";"
    Then the cursor should be at cell (1, 7)
    And I type "2;"
    Then the cursor should be at cell (3, 7)
    When I type "F!"
    And I go to end of buffer
    And I type "2;"
    Then the cursor should be at cell (4, 3)

  Scenario: repeat find opposite
    When I go to beginning of buffer
    And I type "4f "
    And I type ","
    Then the cursor should be at cell (3, 3)
    And I type "2,"
    Then the cursor should be at cell (1, 3)
    When I go to end of buffer
    When I type "3Fa"
    And I type "2,"
    Then the cursor should be at cell (4, 9)

  Scenario: goto char
    And I go to beginning of buffer
    And I type "th"
    Then the cursor should be at cell (1, 8)
    When I type "t2"
    Then the cursor should be at cell (3, 1)
    When I type "2t "
    Then the cursor should be at cell (3, 6)
    When I type "toto"
    Then the cursor should be at cell (4, 0)
    When I type "t@"
    Then the cursor should be at cell (4, 0)
    When I go to end of buffer
    And I type "t@"
    Then the cursor should be at cell (4, 11)

  Scenario: goto char backward
    When I go to end of buffer
    And I type "Tr"
    Then the cursor should be at cell (4, 7)
    When I type "Ti"
    Then the cursor should be at cell (1, 11)
    When I go to end of buffer
    And I type "TaTa"
    Then the cursor should be at cell (4, 10)
    When I go to end of buffer
    And I type "2Ta"
    Then the cursor should be at cell (4, 6)
    When I go to beginning of buffer
    When I type "T@"
    Then the cursor should be at cell (1, 0)

  Scenario: repeat goto char
    When I go to beginning of buffer
    And I type "t "
    And I go to beginning of buffer
    And I type ";"
    Then the cursor should be at cell (1, 2)
    And I type "2;"
    Then the cursor should be at cell (1, 6)
    When I type "T!"
    And I go to end of buffer
    And I type "2;"
    Then the cursor should be at cell (4, 4)

  Scenario: repeat goto char opposite
    When I go to beginning of buffer
    And I type "4t "
    And I type ","
    Then the cursor should be at cell (3, 4)
    And I type "2,"
    Then the cursor should be at cell (1, 8)
    When I go to end of buffer
    When I type "3Ta"
    And I type "2,"
    Then the cursor should be at cell (4, 8)

  Scenario: window top bottom middle
    Given the buffer is empty
    When I insert "1" pages
    And I go to beginning of buffer
    And I type "L"
    Then the current line should be the last line
    When I type "H"
    Then the current line should be "1"
    When I type "M"
    Then the current line should be in the middle of window

  Scenario: window top bottom with prefix
    Given the buffer is empty
    When I insert:
    """
     1
      2
       3
        4
       5
      6
     7
    """
    And I type "H"
    Then the cursor should be at cell (1, 1)
    When I type "3H"
    Then the cursor should be at cell (3, 3)
    When I type "L"
    Then the cursor should be at cell (7, 1)
    When I type "2L"
    Then the cursor should be at cell (6, 2)
    When I type "M"
    Then the cursor should be at cell (4, 4)

  Scenario: go to line
    When I type "G"
    Then the current line should be "7"
    When I type "1G"
    Then the cursor should be at cell (1, 1)
    When I type "5G"
    Then the cursor should be at cell (5, 3)
    When I start an action chain
    And I call "universal-argument"
    And I press "-"
    And I press "3"
    And I press "G"
    And I execute the action chain
    Then the cursor should be at cell (4, 4)

  Scenario: ^ 0 $
    Given the buffer is empty
    When I insert:
    """
    (defun foobar (arg)
      (if arg
        (message "Foo")
      (message "Bar")))
    """
    When I go to word "foobar"
    And I type "^"
    Then the cursor should be at cell (1, 0)
    When I go to word "Foo"
    And I type "^"
    Then the cursor should be at cell (3, 4)
    When I go to word "foobar"
    And I type "0"
    Then the cursor should be at cell (1, 0)
    When I go to word "Foo"
    And I type "0"
    Then the cursor should be at cell (3, 0)
    When I go to word "if"
    And I type "$"
    Then the cursor should be at cell (2, 9)
    When I go to word "Bar"
    And I type "$"
    Then the cursor should be at cell (4, 19)

  Scenario: goto pair
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
      (bar baz))
    """
    And I go to beginning of buffer
    And I type "%"
    Then the cursor should be at cell (2, 11)
    When I type "%"
    Then the cursor should be at cell (1, 0)
    When I go to word "foo"
    And I type "%"
    Then the cursor should be at cell (1, 12)
    When I place the cursor after "bar"
    And I type "%"
    Then the cursor should be at cell (2, 2)
    When I go to end of buffer
    And I type "%"
    Then the cursor should be at cell (1, 0)

  Scenario: search
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
        baz
    foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/bar"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (2, 2)

    When I go to beginning of buffer
    And I start an action chain
    And I type "2/^ *ba."
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (3, 0)

    When I go to beginning of buffer
    And I start an action chain
    And I type "/fo+"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (4, 0)

    And I start an action chain
    And I type "/notmatch"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (4, 0)

    And I start an action chain
    And I type "/FOO"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (4, 0)

  Scenario: wrapped search
    When I go to end of buffer
    And I start an action chain
    And I type "/foo"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (1, 0)
    And I should see message "Search wrapped"

    When I go to cell (2, 3)
    And I start an action chain
    And I type "/bar"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (2, 2)
    And I should see message "Search wrapped"

  Scenario: search current word
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123 456 789
    foobarbaz
    quux qux baz bar foo
    <*> @#$ <> @#$ <*>
    bar()baz
    """
    And I go to word "bar"
    And I press "C-a"
    Then the cursor should be at cell (4, 13)

    When I go to cell (4, 11)
    And I start an action chain
    And I type "2"
    And I press "C-a"
    And I execute the action chain
    Then the cursor should be at cell (1, 8)
    And I should see message "Search wrapped"

    When I go to cell (1, 8)
    And I press "C-a"
    Then the cursor should be at cell (4, 9)

  Scenario: search region string
    Given transient mark mode is active
    When I go to beginning of buffer
    And I set the mark
    And I type "e"
    And I press "C-a"
    Then the cursor should be at cell (3, 0)

    When I go to cell (5, 0)
    And I set the mark
    And I type "f "
    And I press "C-a"
    Then the cursor should be at cell (5, 15)

  Scenario: search next
    Given the buffer is empty
    When I insert:
    """
     foo bar
    baz
     foofoo
    qux
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/foo"
    And I press "RET"
    And I execute the action chain
    And I type "n"
    Then the cursor should be at cell (3, 1)

    When I type "2n"
    Then the cursor should be at cell (1, 1)
    And I should see message "Search wrapped"

  Scenario: search backward
    When I go to end of buffer
    And I start an action chain
    And I type "?foo"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (3, 4)

    When I start an action chain
    And I type "2?foo"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (1, 1)

    When I start an action chain
    And I type "?baz"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (2, 0)
    And I should see message "Search wrapped"

  Scenario: search backward next
    When I go to end of buffer
    And I start an action chain
    And I type "?foo"
    And I press "RET"
    And I type "n"
    And I execute the action chain
    Then the cursor should be at cell (3, 1)

    When I type "n"
    Then the cursor should be at cell (1, 1)

  Scenario: search Next
    When I go to beginning of buffer
    And I start an action chain
    And I type "2/foo"
    And I press "RET"
    And I type "N"
    And I execute the action chain
    Then the cursor should be at cell (1, 1)

    When I type "2N"
    Then the cursor should be at cell (3, 1)
    And I should see message "Search wrapped"

  Scenario: search backward Next
    When I go to end of buffer
    And I start an action chain
    And I type "2? ."
    And I press "RET"
    And I type "N"
    And I execute the action chain
    Then the cursor should be at cell (3, 0)

    When I type "N"
    Then the cursor should be at cell (1, 0)
    And I should see message "Search wrapped"

  Scenario: search next and Next
    Given the buffer is empty
    When I insert:
    """
    foo1
      bar2
        baz3
    qux4
    quux5
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/[a-z]\{3\}[0-9]"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (2, 2)

    When I type "n"
    Then the cursor should be at cell (3, 4)

    When I type "2N"
    Then the cursor should be at cell (1, 0)

    When I type "3n"
    Then the cursor should be at cell (4, 0)

  Scenario: not search (backward) empty
    Given the buffer is empty
    When I insert "foo"
    And I set "mivi--last-search" to "nil"
    And I go to beginning of buffer
    And I start an action chain
    And I type "/"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (1, 0)

    When I go to end of buffer
    And I start an action chain
    And I type "?"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (1, 3)

  Scenario: search (backward) history
    Given the buffer is empty
    When I insert:
    """
    1
    2
    3
    4
    5
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/[24]"
    And I press "RET"
    And I type "/"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (4, 0)

    When I go to end of buffer
    And I start an action chain
    And I type "?[135]"
    And I press "RET"
    And I type "?"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (3, 0)

  Scenario: next line at bot
    Given the buffer is empty
    When I insert:
    """
    foo
     bar
      baz
     qux
    quux
    """
    And I go to beginning of buffer
    And I press "RET"
    Then the cursor should be at cell (2, 1)

    When I start an action chain
    And I type "2"
    And I press "RET"
    And I execute the action chain
    Then the cursor should be at cell (4, 1)

  Scenario: previous line at bot
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
        baz
      qux
    quux
    """
    And I type "-"
    Then the cursor should be at cell (4, 2)

    When I type "2-"
    Then the cursor should be at cell (2, 2)

  Scenario: next sentence
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    1234567890

    foo bar.
        baz qux quux. 123


    4567890
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type ")"
    And I execute the action chain
    Then the cursor should be at cell (3, 0)

    When I start an action chain
    And I type ")"
    And I execute the action chain
    Then the cursor should be at cell (4, 0)

    When I start an action chain
    And I type ")"
    And I execute the action chain
    Then the cursor should be at cell (5, 4)

    When I start an action chain
    And I type ")"
    And I execute the action chain
    Then the cursor should be at cell (5, 18)

    When I start an action chain
    And I type "2)"
    And I execute the action chain
    Then the cursor should be at cell (8, 0)
