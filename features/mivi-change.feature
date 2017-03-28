Feature: Change
  In order to edit text in command mode
  As a user
  I want to change text

  Scenario: change backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux
    qu_ux
    """
    And I type "cb"
    Then I should not see pattern "qu_ux"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "2cb"
    Then I should see pattern "bar-$"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "c3b"
    Then I should see pattern "^-$"
    And the mivi state should be "insert"

  Scenario: change Backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
    """
    And I type "cB"
    Then I should see pattern "baz $"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "2cB"
    Then I should see pattern "^ $"
    And the mivi state should be "insert"

  Scenario: change end of word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       12345 6789
    """
    And I go to beginning of buffer
    And I type "ce"
    Then I should see pattern "^ bar-baz"
    And the mivi state should be "insert"

  Scenario: change End of word
    When I type "cE"
    Then I should see pattern "^ qux-quux"
    And the mivi state should be "insert"

  Scenario: change find
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I go to beginning of buffer
    And I type "cf-"
    Then I should see pattern "^baz "
    And the mivi state should be "insert"

  Scenario: change Find
    When I go to end of buffer
    And I type "c2F3"
    Then I should see pattern "^   12$"
    And the mivi state should be "insert"

  Scenario: change find unmatched
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    """
    And I go to beginning of buffer
    And I type "cfx"
    Then the mivi state should be "command"

  Scenario: change goto char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I go to beginning of buffer
    And I type "ct-"
    Then I should see pattern "^-baz "
    And the mivi state should be "insert"

  Scenario: change goto char backward
    When I go to end of buffer
    And I type "c2T3"
    Then I should see pattern "^   123$"
    And the mivi state should be "insert"

  Scenario: change goto line
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
    And I type "c2G"
    Then I should not see pattern "\(123\|456\)"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"
    When I go to end of buffer
    And I press "<escape>"
    And I type "3cG"
    Then I should not see pattern "\(abc\|def\|ghi\)"
    And the cursor should be at cell (3, 0)
    And the mivi state should be "insert"
    When I go to beginning of buffer
    And I press "<escape>"
    And I type "cG"
    Then the buffer should be empty
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

  Scenario: change backward char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz
    qux
    """
    And I type "ch"
    Then I should see pattern "^qu$"
    And the mivi state should be "insert"

    When I start an action chain
    And I press "<escape>"
    And I press "c"
    And I press "C-h"
    And I execute the action chain
    #Then I should see pattern "^u$"
    #And the mivi state should be "insert"

  Scenario: change forward char
    When I go to beginning of buffer
    And I type "cl"
    Then I should see pattern "^oo "
    And the mivi state should be "insert"

  Scenario: change next line
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
    And I type "cj"
    Then I should not see pattern "\(foo\|bar\)"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "c3j"
    Then I should not see pattern "\(baz\|qux\|quux\)"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"
    When I go to line "3"
    And I press "<escape>"
    And I type "3cj"
    Then I should not see pattern "[1-4]"
    And the cursor should be at cell (3, 0)
    And the mivi state should be "insert"

  Scenario: change previous line
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
    And I type "ck"
    Then I should not see pattern "[12]"
    And the cursor should be at cell (9, 0)
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "c4k"
    Then I should not see pattern "\([345]\|quux\)"
    And the cursor should be at cell (5, 0)
    And the mivi state should be "insert"
    When I go to line "3"
    And I press "<escape>"
    And I type "2ck"
    Then I should not see pattern "\(foo\|bar\|baz\)"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

  Scenario: change forward word
    Given the buffer is empty
    When I insert:
    """
    foo   bar-baz
      qux quux
     123.456
    """
    And I go to beginning of buffer
    And I type "cw"
    Then I should see pattern "^   bar-"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "c3w"
    Then I should see pattern "^baz$"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "cw"
    Then I should see pattern "^$"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "4cw"
    Then I should see pattern "^\.456$"
    And the mivi state should be "insert"

  Scenario: change forward Word
    Given the buffer is empty
    When I insert:
    """
    foo   bar-baz baz
      qux quux
     123.456
    """
    And I go to beginning of buffer
    And I type "cW"
    Then I should see pattern "^   bar-"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "c2W"
    Then I should see pattern "^ baz$"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "5cW"
    Then the buffer should be empty

  Scenario: change end of line
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    123
    """
    And I place the cursor after "foo"
    And I type "c$"
    Then I should see pattern "^foo$"
    And the mivi state should be "insert"

  Scenario: change end of line by C
    When I place the cursor after "2"
    And I type "C"
    Then I should see pattern "^12$"
    And the mivi state should be "insert"

  Scenario: change beginning of line
    Given the buffer is empty
    When I insert:
    """
    foo bar
      baz
    """
    And I place the cursor after "foo"
    And I type "c0"
    Then I should see pattern "^ bar$"
    And the mivi state should be "insert"

  Scenario: change beginning of text
    When I place the cursor after "baz"
    And I type "c^"
    Then I should see pattern "^  $"
    And the mivi state should be "insert"

  Scenario: change line
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
    And I type "cc"
    Then I should not see pattern "^foo"
    Then I should see pattern "^$"
    And the current line should be "1"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I type "3cc"
    Then I should not see pattern "\(bar\|baz\)"
    And the current line should be "1"
    And the mivi state should be "insert"
    When I press "<escape>"
    And I go to line "2"
    And I type "c2c"
    Then I should not see pattern "\(qux\|quux\)"
    And the current line should be "2"
    And the mivi state should be "insert"
    When I go to end of buffer
    And I press "<escape>"
    And I call "universal-argument"
    And I type "-1cc"
    Then I should not see pattern "\(end\|2\)"
    And I should see pattern "^1$"
    And the current line should be "4"
    And the mivi state should be "insert"

  Scenario: change find repeat
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
      baz bar foo
    """
    And I go to beginning of buffer
    And I type "fb"
    And I type "c;"
    Then I should see pattern "^foo az$"
    And the mivi state should be "insert"

  Scenario: change find repeat opposite
    When I go to end of buffer
    And I type "2c,"
    Then I should see pattern "^  $"
    And the mivi state should be "insert"

  Scenario: change window top
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I go to line "2"
    And I type "cH"
    Then I should not see pattern "\(foo\|bar\)"
    And I should see pattern "^baz$"
    And the mivi state should be "insert"

  Scenario: change window middle
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I go to line "2"
    And I type "cM"
    Then I should not see pattern "^bar$"
    And I should see pattern "^foo$"
    And I should see pattern "^baz$"
    And the mivi state should be "insert"

  Scenario: change window bottom
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I go to line "2"
    And I type "cL"
    Then I should not see pattern "\(bar\|baz\)"
    And I should see pattern "^foo$"
    And the mivi state should be "insert"

  Scenario: change goto pair
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
      (bar baz))
    """
    And I go to cell (2, 2)
    And I type "c%"
    Then I should see pattern "^  )$"
    And the mivi state should be "insert"
    When I go to cell (1, 12)
    And I press "<escape>"
    And I type "c%"
    Then I should see pattern "foo $"
    And the mivi state should be "insert"
    When I go to end of buffer
    And I press "<escape>"
    And I type "c%"
    Then the buffer should be empty
    And the mivi state should be "insert"

  Scenario: change search
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    baz bar foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "c/bar"
    And I press "RET"
    And I execute the action chain
    Then I should see pattern "^bar baz$"
    And the mivi state should be "insert"

    When I start an action chain
    And I press "<escape>"
    And I type "2c/baz"
    And I press "RET"
    And I execute the action chain
    Then I should see pattern "^baz bar foo$"
    And the mivi state should be "insert"

  Scenario: change search current word
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    bar foo bar qux
    """
    And I go to word "bar"
    And I start an action chain
    And I type "2c"
    And I press "C-a"
    And I execute the action chain
    Then I should see pattern "^foo bar qux$"
    And the mivi state should be "insert"

  Scenario: change search next
    Given the buffer is empty
    When I insert:
    """
    foo bar baz bar foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "/bar"
    And I press "RET"
    And I type "cn"
    And I execute the action chain
    Then I should see pattern "^foo bar foo$"
    And the mivi state should be "insert"

  Scenario: change search backward next
    Given the buffer is empty
    When I insert:
    """
    foo bar baz bar foo
    """
    When I start an action chain
    And I type "?ba."
    And I press "RET"
    And I type "cn"
    And I execute the action chain
    Then I should see pattern "^foo bar bar foo$"
    And the mivi state should be "insert"

  Scenario: change search backward
    Given the buffer is empty
    When I insert:
    """
    foo bar baz qux quux
    """
    When I go to word "qux"
    When I start an action chain
    And I type "c?bar"
    And I press "RET"
    And I execute the action chain
    Then I should see pattern "^foo qux quux$"
    And the mivi state should be "insert"

  Scenario: change search Next
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    qux quux foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I type "2/qu+x"
    And I press "RET"
    And I type "cN"
    And I execute the action chain
    Then I should see pattern "^quux foo$"
    And the mivi state should be "insert"

    When I go to end of buffer
    And I start an action chain
    And I press "<escape>"
    And I type "2?ba."
    And I press "RET"
    And I type "cN"
    And I execute the action chain
    Then I should see pattern "^foo baz$"
    And the mivi state should be "insert"

  Scenario: change next line at bot
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
        baz
    """
    And I go to cell (1, 2)
    And I type "c"
    And I press "RET"
    Then I should not see pattern "\(foo\|bar\)"
    And I should see:
    """

        baz
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

  Scenario: change previous line at bot
    Given the buffer is empty
    When I insert:
    """

    foo
      bar
        baz
    """
    And I go to cell (3, 2)
    And I type "c-"
    Then I should not see pattern "\(foo\|bar\)"
    And I should see:
    """


        baz
    """
    And the cursor should be at cell (2, 0)
    And the mivi state should be "insert"

  Scenario: change next sentence
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    1234567890

    foo bar.
        baz qux quux. 123


    4567890
    """
    And I go to cell (1, 6)
    And I start an action chain
    And I type "c)"
    And I execute the action chain
    Then I should see:
    """
    foo ba
    foo bar.
    """
    And the cursor should be at cell (1, 6)
    And the mivi state should be "insert"

    When I start an action chain
    And I press "<escape>"
    And I type "c3)"
    And I execute the action chain
    Then I should see:
    """
    foo b

    4567890
    """
    And the cursor should be at cell (1, 5)
    And the mivi state should be "insert"

  Scenario: change previous sentence
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    1234567890

    foo bar.
    """
    And I start an action chain
    And I type "c("
    And I execute the action chain
    Then I should not see pattern "^foo bar\.$"
    And the cursor should be at cell (4, 0)
    And the mivi state should be "insert"

    When I start an action chain
    And I press "<escape>"
    And I type "2c("
    And I execute the action chain
    Then the buffer should be empty
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

  Scenario: change next paragraph
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
    And I type "c}"
    Then I should see:
    """

    foo bar.
    """
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

    When I press "<escape>"
    And I type "2c}"
    Then the buffer should be empty
    And the mivi state should be "insert"

  Scenario: change previous paragraph
    Given the buffer is empty
    When I insert:
    """
    foo bar baz
    1234567890

    foo bar.
        baz qux quux. 123


    4567890
    """
    And I type "c{"
    Then I should not see:
    """

    4567890
    """
    And the cursor should be at cell (7, 0)
    And the mivi state should be "insert"

    When I press "<escape>"
    And I type "2c{"
    Then the buffer should be empty
    And the mivi state should be "insert"

  Scenario: change goto mark
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
        baz
    """
    And I go to cell (2, 3)
    And I type "m`"
    And I go to beginning of buffer
    And I type "c``"
    Then I should see pattern "^ar$"
    And the mivi state should be "insert"

    When I press "<escape>"
    And I go to cell (2, 1)
    And I type "mT"
    And I go to end of buffer
    And I type "c`T"
    Then I should see pattern "^ $"
    And the mivi state should be "insert"

  Scenario: change goto mark line
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
        baz
          123
            456
    """
    And I go to cell (2, 3)
    And I type "m'"
    And I go to beginning of buffer
    And I type "c''"
    Then I should not see pattern "\(foo\|bar\)"
    And I should see pattern "^    baz$"
    And the cursor should be at cell (1, 0)
    And the mivi state should be "insert"

    When I press "<escape>"
    And I go to word "123"
    And I type "m9"
    And I go to end of buffer
    And I type "c'9"
    Then I should not see pattern "\(123\|456\)"
    And I should see pattern "^    baz$"
    And the cursor should be at cell (3, 0)
    And the mivi state should be "insert"
