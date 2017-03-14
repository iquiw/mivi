Feature: Undo
  In order to edit text quickly
  As a user
  I want to repeat previous command

  Scenario: repeat insert
    Given the buffer is empty
    When I type "ifoo"
    And I press "<escape>"
    And I type "."
    Then I should see "fofooo"
    And the cursor should be at cell (1, 4)

    When I type ".."
    Then I should see "fofofofooooo"
    And the cursor should be at cell (1, 8)

  Scenario: repeat insert with current prefix
    Given the buffer is empty
    When I type "ibar"
    And I press "RET"
    When I type "baz"
    And I press "<escape>"
    And I type "2."
    Then I should see:
    """
    bar
    babar
    bazbar
    bazz
    """
    And the cursor should be at cell (4, 2)

    Given the buffer is empty
    And I type "."
    Then I should see:
    """
    bar
    bazbar
    baz
    """
    And the cursor should be at cell (3, 2)

  Scenario: repeat insert empty
    Given the buffer is empty
    When I insert "foo"
    And I type "i"
    And I press "<escape>"
    And I type "."
    Then I should see pattern "^foo$"
    And the cursor should be at cell (1, 1)

  Scenario: repeat insert pair
    Given the buffer is empty
    When I type "i(foo"
    # This emulates electric pair mode
    And I insert ")"
    And I press "<escape>"
    Then I should see "(foo)"
    When I type "."
    Then I should see "(foo(foo))"

  Scenario: repeat insert after delete
    Given the buffer is empty
    When I insert "foobar"
    And I go to cell (1, 3)
    And I type "i"
    And I press "C-d"
    And I press "C-d"
    And I press "C-d"
    And I insert "123"
    And I press "<escape>"
    Then I should see "foo123"
    When I type "."
    Then I should see "foo121233"

  Scenario: repeat insert after delete backward
    Given the buffer is empty
    When I insert "foobar"
    And I go to cell (1, 3)
    And I type "i"
    And I press "<backspace>"
    And I press "<backspace>"
    And I press "<backspace>"
    And I insert "123"
    And I press "<escape>"
    Then I should see "123bar"
    When I type "."
    Then I should see "121233bar"

  Scenario: repeat Insert
    Given the buffer is empty
    When I insert:
    """
       1
       2
       3
    """
    And I go to beginning of buffer
    And I type "Ifoo"
    And I press "<escape>"
    And I type "."
    Then I should see pattern "^   foofoo1$"
    And the cursor should be at cell (1, 5)

    When I go to line "2"
    And I type "."
    Then I should see pattern "^   foo2$"
    And the cursor should be at cell (2, 5)

    When I go to line "3"
    And I type "2."
    Then I should see pattern "^   foofoo3$"
    And the cursor should be at cell (3, 8)

  Scenario: repeat open
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
    """
    And I type "obar"
    And I press "<escape>"
    And I type "."
    Then I should see:
    """
      bar
      bar
    """
    And the cursor should be at cell (3, 4)

    When I type "2."
    Then I should see:
    """
      bar
      bar
      bar
      bar
    """
    And the cursor should be at cell (5, 4)

  Scenario: repeat Open
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
      bar
    """
    And I type "Obaz"
    And I press "<escape>"
    And I type "."
    Then I should see:
    """
      baz
      baz
      bar
    """
    And the cursor should be at cell (2, 4)

    When I type "2."
    Then I should see:
    """
      baz
      baz
      baz
      baz
      bar
    """
    And the cursor should be at cell (3, 4)

  Scenario: repeat append
    Given the buffer is empty
    When I type "afoo"
    And I press "<escape>"
    And I type "."
    Then I should see "foofoo"
    And the cursor should be at cell (1, 5)

    When I type "3."
    Then I should see "foofoofoofoofoo"
    And the cursor should be at cell (1, 14)

  Scenario: repeat Append
    Given the buffer is empty
    When I type "Afoo"
    And I press "<escape>"
    And I go to beginning of buffer
    And I type "."
    Then I should see "foofoo"
    And the cursor should be at cell (1, 5)

    When I type "Abar"
    And I press "RET"
    When I type "baz"
    And I press "<escape>"
    And I type "2."
    Then I should see:
    """
    foofoobar
    bazbar
    bazbar
    baz
    """
    And the cursor should be at cell (4, 2)

  Scenario: repeat Append and back point
    Given the buffer is empty
    When I insert "foo"
    And I type "Abar"
    And I go to beginning of buffer
    And I press "<escape>"
    And I type "."
    Then I should see pattern "^foobar$"
    And the cursor should be at cell (1, 0)

  Scenario: repeat replace char
    Given the buffer is empty
    When I insert:
    """
    foobar
    """
    And I go to beginning of buffer
    And I type "rx"
    And I go to cell (1, 1)
    And I type "."
    Then I should see "xxobar"

    When I go to cell (1, 3)
    And I type "2."
    Then I should see "xxoxxr"

    When I type "3ry"
    And I go to beginning of buffer
    And I type "."
    Then I should see "yyyyyy"

  Scenario: repeat Replace
    Given the buffer is empty
    When I insert:
    """
    foo
    1234567
    xxxxxxxxxxxxx
    z
    """
    And I go to beginning of buffer
    And I type "Rbar"
    And I press "<escape>"
    And I go to line "2"
    And I type "."
    Then I should see pattern "^bar4567$"
    And the cursor should be at cell (2, 2)

    When I go to cell (3, 3)
    And I type "3."
    Then I should see pattern "^xxxbarbarbarxxxxxxx$"
    And the cursor should be at cell (3, 11)

    When I go to cell (4, 0)
    And I type "."
    Then I should see:
    """
    bar
    bar4567
    xxxbarbarbarxxxxxxx
    barbarbar
    """
    And the cursor should be at cell (4, 8)

  Scenario: repeat substitute
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I go to beginning of buffer
    And I type "s123"
    And I press "<escape>"
    And I go to word "bar"
    And I type "."
    Then I should see pattern "^123oo 123ar"
    And the cursor should be at cell (1, 8)

    And I go to word "baz"
    And I type "3."
    Then I should see pattern "^123oo 123ar 123$"
    And the cursor should be at cell (1, 14)

    And I go to cell (1, 3)
    And I type "."
    Then I should see pattern "^123123123ar 123$"
    And the cursor should be at cell (1, 5)

  Scenario: repeat kill char
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I go to beginning of buffer
    And I type "x."
    Then I should see pattern "^o bar baz$"

    When I type "2x."
    Then I should see pattern "^r baz$"

    When I type "3."
    Then I should see pattern "^az$"

  Scenario: repeat kill backward char
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I type "X."
    Then I should see pattern "^foo bar b$"

    When I type "2X."
    Then I should see pattern "^foo b$"

    When I type "3."
    Then I should see pattern "^fo$"

  Scenario: repeat delete
    Given the buffer is empty
    When I insert:
    """
    foo
    bar baz
    """
    And I go to beginning of buffer
    And I type "dw"
    Then I should see pattern "^$"
    When I go to line "2"
    And I type "."
    Then I should see pattern "^baz$"

  Scenario: repeat delete with previous prefix
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123 456 789
    """
    And I go to beginning of buffer
    And I type "2de"
    Then I should see pattern "^ baz$"
    When I go to line "2"
    And I type "."
    Then I should see pattern "^ 789$"

  Scenario: repeat delete with current prefix
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I type "db"
    Then I should see pattern "^foo bar $"
    When I type "2."
    Then the buffer should be empty

    When I insert:
    """
    foo bar baz
    """
    And I type "."
    Then I should see pattern "^foo $"

  Scenario: repeat delete find
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    qux quux 0
    """
    And I go to beginning of buffer
    And I type "df "
    Then I should see pattern "^bar baz$"
    When I type "."
    Then I should see pattern "^baz$"
    When I go to line "2"
    And I type "2."
    Then I should see pattern "^0$"

  Scenario: repeat delete Find
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I type "dF "
    Then I should see pattern "^foo bar$"
    When I type "."
    Then I should see pattern "^foo$"

  Scenario: repeat delete goto char
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I go to beginning of buffer
    And I type "dt "
    Then I should see pattern "^ bar baz$"
    When I type "."
    Then I should see pattern "^ baz$"

  Scenario: repeat delete goto char backward
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I type "dT "
    Then I should see pattern "^foo bar $"
    When I type "2."
    Then I should see pattern "^foo $"

  Scenario: repeat delete search
    Given the buffer is empty
    When I insert:
    """
    1 2 3 4 5
    6 7 8 9 10
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "d/[39]"
    And I press "RET"
    And I type "."
    And I execute the action chain
    Then I should see pattern "^9 10$"

  Scenario: repeat delete search backward
    Given the buffer is empty
    When I insert:
    """
    1 2 3 4 5
    6 7 8 9 10
    """
    And I start an action chain
    And I type "d?[46]"
    And I press "RET"
    And I type "."
    And I execute the action chain
    Then I should see pattern "^1 2 3 $"

  Scenario: repeate delete line
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    5 quux
    """
    And I go to beginning of buffer
    And I type "dd."
    Then I should not see pattern "\(1\|2\)"

    When I type "2."
    Then I should not see pattern "\(3\|4\)"

  Scenario: repeat change
    Given the buffer is empty
    When I insert:
    """
    foo bar
    1234567890
    """
    And I go to beginning of buffer
    And I type "cwquux"
    And I press "<escape>"
    And I go to word "bar"
    And I type "."
    Then I should see pattern "^quux quux$"
    And the cursor should be at cell (1, 8)

    When I go to line "2"
    And I type "."
    Then I should see:
    """
    quux quux
    quux
    """
    And the cursor should be at cell (2, 3)

  Scenario: repeat change with previous prefix
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123 456 789
    """
    And I go to beginning of buffer
    And I type "2cequux"
    And I press "<escape>"
    Then I should see pattern "^quux baz$"
    When I go to line "2"
    And I type "."
    Then I should see pattern "^quux 789$"

  Scenario: repeat change with current prefix
    Given the buffer is empty
    When I insert:
    """
    foo bar quux
    """
    And I type "cbquux"
    And I press "<escape>"
    Then I should see pattern "^foo bar quux$"
    When I type "3."
    Then I should see pattern "^quuxx$"

    Given the buffer is empty
    And I insert:
    """
    1 2 3
    """
    And I type "."
    Then I should see pattern "^quux$"

  Scenario: repeat change find
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123 456 789
    """
    And I go to beginning of buffer
    And I type "2cf hello"
    And I press "<escape>"
    Then I should see pattern "^hellobaz$"
    When I go to line "2"
    And I type "."
    Then I should see pattern "^hello789$"

  Scenario: repeat change Find
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I type "cFbqux"
    And I press "<escape>"
    Then I should see pattern "^foo bar qux$"
    And I type "."
    Then I should see pattern "^foo quxx$"

  Scenario: repeat change search
    Given the buffer is empty
    When I insert:
    """
    1 2 3 4 5
    6 7 8 9 10
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "c/[14]"
    And I press "RET"
    And I type "foo bar"
    And I press "<escape>"
    And I type "2."
    And I execute the action chain
    Then I should see pattern "^foo bafoo bar10$"

  Scenario: repeat change search backward
    Given the buffer is empty
    When I insert:
    """
    1 2 3 4 5
    6 7 8 9 10
    """
    And I start an action chain
    And I type "c?[57]"
    And I press "RET"
    And I type "hello"
    And I press "<escape>"
    And I type "."
    And I execute the action chain
    Then I should see pattern "^1 2 3 4 helloo$"

  Scenario: repeat change line
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    5 quux
    """
    And I go to beginning of buffer
    And I type "cchello"
    And I press "<escape>"
    And I go to line "2"
    And I type "."
    Then I should see:
    """
    hello
    hello
    3 baz
    4 qux
    5 quux
    """

    When I type "2."
    Then I should see:
    """
    hello
    hello
    4 qux
    5 quux
    """

    When I type "."
    Then I should see:
    """
    hello
    hello
    5 quux
    """

  Scenario: not repeat delete goto mark
    Given the buffer is empty
    When I insert:
    """
    1
    2
    3
    4
    """
    And I type "ma"
    And I go to line "3"
    And I type "d`a"
    And I go to beginning of buffer
    And I type "."
    Then I should see:
    """
    1
    2
    """

  Scenario: not repeat delete goto mark line
    Given the buffer is empty
    When I insert:
    """
    1
    2
    3
    4
    """
    And I type "mb"
    And I go to line "3"
    And I type "d'b"
    And I go to beginning of buffer
    And I type "."
    Then I should see:
    """
    1
    2
    """

  Scenario: not repeat change goto mark
    Given the buffer is empty
    When I insert:
    """
    1
    2
    3
    4
    """
    And I type "ma"
    And I go to line "3"
    And I type "c`afoo"
    And I press "<escape>"
    And I go to beginning of buffer
    And I type "."
    Then I should see:
    """
    1
    2
    foo
    """

  Scenario: not repeat change goto mark line
    Given the buffer is empty
    When I insert:
    """
    1
    2
    3
    4
    """
    And I type "mb"
    And I go to line "3"
    And I type "c'bbar"
    And I go to beginning of buffer
    And I type "."
    Then I should see:
    """
    1
    2
    bar
    """
