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
    """
    And I go to end of buffer
    And I type "b"
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
    And I type "2b"
    Then the cursor should be at cell (1, 0)

  Scenario: end of word
    When I go to beginning of buffer
    And I type "e"
    Then the cursor should be at cell (1, 2)
    When I type "e"
    Then the cursor should be at cell (1, 6)
    When I type "e"
    Then the cursor should be at cell (1, 11)
    When I type "e"
    Then the cursor should be at cell (3, 4)
    When I type "e"
    Then the cursor should be at cell (3, 10)
    When I type "3e"
    Then the cursor should be at cell (5, 2)

  Scenario: forward word
    When I go to beginning of buffer
    And I type "w"
    Then the cursor should be at cell (1, 4)
    And I type "w"
    Then the cursor should be at cell (1, 9)
    And I type "w"
    Then the cursor should be at cell (3, 2)
    And I type "3w"
    Then the cursor should be at cell (3, 11)
    And I type "6w"
    Then the cursor should be at cell (5, 7)

  Scenario: Backward word
    When I go to end of buffer
    And I type "B"
    Then the cursor should be at cell (5, 0)
    And I type "B"
    Then the cursor should be at cell (3, 16)
    And I type "3B"
    Then the cursor should be at cell (1, 4)

  Scenario: End of word
    When I go to beginning of buffer
    And I type "3E"
    Then the cursor should be at cell (1, 11)
    And I type "E"
    Then the cursor should be at cell (3, 14)
    And I type "E"
    Then the cursor should be at cell (3, 20)

  Scenario: forward of Word
    When I go to beginning of buffer
    And I type "W"
    Then the cursor should be at cell (1, 4)
    And I type "3W"
    Then the cursor should be at cell (3, 16)
    And I type "W"
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
