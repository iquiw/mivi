Feature: Copy
  In order to edit text in command mode
  As a user
  I want to copy text

  Scenario: copy backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux
    qu_ux
    """
    And I type "yb"
    Then the current kill-ring should be "qu_ux"
    And the cursor should be at cell (2, 0)
    And I should see pattern "^qu_ux$"
    And the mivi state should be "command"
    When I type "2yb"
    Then the current kill-ring should be:
    """
    baz qux

    """
    And the cursor should be at cell (1, 8)
    And the mivi state should be "command"
    When I type "y3b"
    Then the current kill-ring should be "foo bar-"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy Backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux 123;456
    """
    And I type "yB"
    Then the current kill-ring should be "123;456"
    And the cursor should be at cell (1, 21)
    And the mivi state should be "command"
    When I type "2yB"
    Then the current kill-ring should be "bar-baz qux-quux "
    And the cursor should be at cell (1, 4)
    And the mivi state should be "command"
    When I go to end of buffer
    When I type "y4B"
    Then the current kill-ring should be "foo bar-baz qux-quux 123;456"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy end of word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       12345 6789
    """
    And I go to beginning of buffer
    And I type "ye"
    Then the current kill-ring should be "foo"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy End of word
    When I type "2yE"
    Then the current kill-ring should be "foo bar-baz"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy find
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I go to beginning of buffer
    And I type "yf-"
    Then the current kill-ring should be "foo bar-"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy Find
    When I go to end of buffer
    And I type "y2F3"
    Then the current kill-ring should be "3454321"
    And the cursor should be at cell (2, 5)
    And the mivi state should be "command"

  Scenario: copy goto char
    When I go to beginning of buffer
    And I type "yt-"
    Then the current kill-ring should be "foo bar"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy goto char backward
    When I go to end of buffer
    And I type "y2T3"
    Then the current kill-ring should be "454321"
    And the cursor should be at cell (2, 6)
    And the mivi state should be "command"

  Scenario: copy goto line
    Given the buffer is empty
    When I insert:
    """
    123
     456
      789
       0
      abc
     def
    ghi
    """
    And I go to beginning of buffer
    And I type "y2G"
    Then the current kill-ring should be:
    """
    123
     456

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to end of buffer
    And I type "3yG"
    Then the current kill-ring should be:
    """
      789
       0
      abc
     def
    ghi
    """
    And the cursor should be at cell (3, 2)
    And the mivi state should be "command"
    When I go to beginning of buffer
    And I type "yG"
    Then the current kill-ring should be:
    """
    123
     456
      789
       0
      abc
     def
    ghi
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy backward char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz
    """
    And I type "yh"
    Then the current kill-ring should be "z"
    And the cursor should be at cell (1, 10)
    And the mivi state should be "command"

    When I start an action chain
    And I press "y"
    And I press "C-h"
    And I execute the action chain
    Then the current kill-ring should be "a"
    And the cursor should be at cell (1, 9)
    And the mivi state should be "command"

  Scenario: copy forward char
    When I go to beginning of buffer
    And I type "yl"
    Then the current kill-ring should be "f"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy next line
    Given the buffer is empty
    When I insert:
    """
    foo
     bar
      baz
       qux
        quux
        5
       4
      3
     2
    1
    """
    And I go to beginning of buffer
    And I type "yj"
    Then the current kill-ring should be:
    """
    foo
     bar

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to word "baz"
    And I type "y3j"
    Then the current kill-ring should be:
    """
      baz
       qux
        quux
        5

    """
    And the cursor should be at cell (3, 2)
    And the mivi state should be "command"
    When I go to word "4"
    And I type "3yj"
    Then the current kill-ring should be:
    """
       4
      3
     2
    1
    """
    And the cursor should be at cell (7, 3)
    And the mivi state should be "command"

  Scenario: copy previous line
    When I go to end of buffer
    And I type "yk"
    Then the current kill-ring should be:
    """
     2
    1
    """
    And the cursor should be at cell (9, 1)
    And the mivi state should be "command"
    When I type "y3k"
    Then the current kill-ring should be:
    """
        5
       4
      3
     2

    """
    And the cursor should be at cell (6, 1)
    And the mivi state should be "command"
    When I go to line "3"
    And I type "2yk"
    Then the current kill-ring should be:
    """
    foo
     bar
      baz

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy forward word
    Given the buffer is empty
    When I insert:
    """
    foo   bar-baz
      qux quux
     123.456
    """
    And I go to beginning of buffer
    And I type "yw"
    Then the current kill-ring should be "foo   "
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I type "y2w"
    Then the current kill-ring should be "foo   bar"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to word "bar"
    And I type "5yw"
    Then the current kill-ring should be:
    """
    bar-baz
      qux quux
     
    """
    And the cursor should be at cell (1, 6)
    And the mivi state should be "command"

  Scenario: copy forward Word
    When I go to beginning of buffer
    And I type "yW"
    Then the current kill-ring should be "foo   "
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I type "y2W"
    Then the current kill-ring should be:
    """
    foo   bar-baz
      
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to word "bar"
    And I type "5yW"
    Then the current kill-ring should be:
    """
    bar-baz
      qux quux
     123.456
    """
    And the cursor should be at cell (1, 6)
    And the mivi state should be "command"

  Scenario: copy end of line
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123
        456
             789
    0
    """
    And I place the cursor after "foo"
    And I type "y$"
    Then the current kill-ring should be " bar baz"
    And the cursor should be at cell (1, 3)
    And the mivi state should be "command"
    When I go to beginning of buffer
    And I type "y2$"
    Then the current kill-ring should be:
    """
    foo bar baz
    123
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I type "4y$"
    Then the current kill-ring should be:
    """
    foo bar baz
    123
        456
             789
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy beginning of line
    Given the buffer is empty
    When I insert:
    """
    foo bar
      baz
    1
      2
        3
    """
    And I place the cursor after "foo"
    And I type "y0"
    Then the current kill-ring should be "foo"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I type "2y0"
    Then the current kill-ring should be:
    """
    foo bar

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy beginning of text
    When I place the cursor after "foo"
    And I type "y^"
    Then the current kill-ring should be "foo"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I place the cursor after "baz"
    And I type "y^"
    Then the current kill-ring should be "baz"
    And the cursor should be at cell (2, 2)
    And the mivi state should be "command"

  Scenario: copy line
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
     baz
    qux
        quux
    1
    2
    end
    """
    And I go to beginning of buffer
    And I type "yy"
    Then the current kill-ring should be:
    """
    foo

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I type "2yy"
    Then the current kill-ring should be:
    """
    foo
      bar

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to word "baz"
    And I type "y3y"
    Then the current kill-ring should be:
    """
     baz
    qux
        quux

    """
    And the cursor should be at cell (3, 1)
    And the mivi state should be "command"
    When I go to word "2"
    And I call "universal-argument"
    And I type "-1yy"
    Then the current kill-ring should be:
    """
    1
    2

    """
    And the cursor should be at cell (7, 0)
    And the mivi state should be "command"

  Scenario: copy line by Y
    When I go to beginning of buffer
    And I type "Y"
    Then the current kill-ring should be:
    """
    foo

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"
    When I go to word "quux"
    And I type "4Y"
    Then the current kill-ring should be:
    """
        quux
    1
    2
    end
    """
    And the cursor should be at cell (5, 4)
    And the mivi state should be "command"

  Scenario: copy find repeat
    Given the buffer is empty
    When I insert:
    """
    foo bar
    baz
      baz bar
      foo
    """
    And I go to beginning of buffer
    And I type "fb"
    And I type "y;"
    Then the current kill-ring should be:
    """
    bar
    b
    """
    And the cursor should be at cell (1, 4)
    And the mivi state should be "command"
    When I type "y2;"
    Then the current kill-ring should be:
    """
    bar
    baz
      b
    """
    And the cursor should be at cell (1, 4)
    And the mivi state should be "command"
    When I go to end of buffer
    And I type "Fo"
    And I type "3y;"
    Then the current kill-ring should be:
    """
    oo bar
    baz
      baz bar
      fo
    """
    And the cursor should be at cell (1, 1)
    And the mivi state should be "command"

  Scenario: copy find repeat opposite
    When I go to beginning of buffer
    And I type "3tb"
    And I type "y,"
    Then the current kill-ring should be:
    """
    az
     
    """
    And the cursor should be at cell (2, 1)
    And the mivi state should be "command"
    When I type "Ta"
    And I type "y2,"
    Then the current kill-ring should be:
    """
    r
    baz
      b
    """
    And the cursor should be at cell (1, 6)
    And the mivi state should be "command"
    When I go to beginning of buffer
    And I type "T "
    And I type "3y,"
    Then the current kill-ring should be:
    """
    foo bar
    baz
     
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy window top
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I go to line "2"
    And I type "yH"
    Then the current kill-ring should be:
    """
    foo
    bar

    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy window middle
    When I go to cell (2, 2)
    And I type "yM"
    Then the current kill-ring should be:
    """
    bar

    """
    And the cursor should be at cell (2, 0)
    And the mivi state should be "command"

  Scenario: copy window bottom
    When I go to cell (2, 2)
    And I type "yL"
    Then the current kill-ring should be:
    """
    bar
    baz
    """
    And the cursor should be at cell (2, 2)
    And the mivi state should be "command"

  Scenario: copy goto pair
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
      (bar baz))
    """
    And I go to cell (2, 2)
    And I type "y%"
    Then the current kill-ring should be "(bar baz)"
    And the cursor should be at cell (2, 2)
    And the mivi state should be "command"
    When I go to cell (1, 12)
    And I type "y%"
    Then the current kill-ring should be "()"
    And the cursor should be at cell (1, 11)
    And the mivi state should be "command"
    When I go to end of buffer
    And I type "y%"
    Then the current kill-ring should be:
    """
    (defun foo ()
      (bar baz))
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "command"

  Scenario: copy search
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    baz bar foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "y/bar"
    And I press "RET"
    And I execute the action chain
    Then the current kill-ring should be "foo "
    And the mivi state should be "command"

    When I start an action chain
    And I type "2y/baz"
    And I press "RET"
    And I execute the action chain
    Then the current kill-ring should be:
    """
    foo bar baz

    """
    And the mivi state should be "command"

  Scenario: copy search next
    Given the buffer is empty
    When I insert:
    """
    foo bar baz bar foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/bar"
    And I press "RET"
    And I type "yn"
    And I execute the action chain
    Then the current kill-ring should be "bar baz "
    And the mivi state should be "command"

  Scenario: copy search backward next
    Given the buffer is empty
    When I insert:
    """
    foo bar baz bar foo
    """
    When I start an action chain
    And I type "?ba."
    And I press "RET"
    And I type "yn"
    And I execute the action chain
    Then the current kill-ring should be "baz "
    And the mivi state should be "command"

  Scenario: copy search backward
    Given the buffer is empty
    When I insert:
    """
    foo bar baz qux quux
    """
    When I go to word "qux"
    When I start an action chain
    And I type "y?bar"
    And I press "RET"
    And I execute the action chain
    Then the current kill-ring should be "bar baz "
    And the mivi state should be "command"
