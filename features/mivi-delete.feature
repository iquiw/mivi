Feature: Delete
  In order to edit text in command mode
  As a user
  I want to delete text

  Scenario: delete backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux
    quux
    """
    And I type "db"
    Then I should not see pattern "quux"
    When I type "2db"
    Then I should see pattern "bar-$"
    When I type "d2b"
    Then the buffer should be empty

  Scenario: delete Backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
    """
    And I type "dB"
    Then I should not see pattern "baz$"
    When I type "2dB"
    Then the buffer should be empty
    When I type "dB"
    And I type "p"
    Then I should see pattern "^foo bar-baz $"

  Scenario: delete end of word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       12345 6789
    """
    And I go to beginning of buffer
    And I type "de"
    Then I should see pattern "^ bar-baz"
    When I type "3de"
    Then I should see pattern "^-quux$"
    When I type "d2e"
    Then I should see pattern "^ 6789$"

  Scenario: delete End of word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       12345 6789
    """
    And I go to beginning of buffer
    And I type "d2E"
    Then I should see pattern "^ qux-quux"
    When I type "2dE"
    Then I should see pattern "^ 6789$"
    When I type "dE"
    Then the buffer should be empty

  Scenario: delete find
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I go to beginning of buffer
    And I type "df-"
    Then I should see pattern "^baz "
    When I type "3dfu"
    Then I should see pattern "^x$"
    When I type "d2f1"
    Then the buffer should be empty

  Scenario: delete Find
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I type "d2F3"
    Then I should see pattern "^   12$"
    When I type "2dF-"
    Then I should see pattern " bar$"
    When I type "dFr"
    Then I should see pattern " ba$"

  Scenario: delete goto char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I go to beginning of buffer
    And I type "dt-"
    Then I should see pattern "^-baz "
    When I type "3dtu"
    Then I should see pattern "^ux$"
    When I type "d2t1"
    Then I should see pattern "^1$"

  Scenario: delete goto char backward
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       123454321
    """
    And I type "d2T3"
    Then I should see pattern "^   123$"
    When I type "2dT-"
    Then I should see pattern " bar-$"
    When I type "dTr"
    Then I should see pattern " bar$"

  Scenario: delete goto line
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
    And I type "d2G"
    Then I should not see pattern "\(123\|456\)"
    And the cursor should be at cell (1, 2)
    When I go to end of buffer
    And I type "3dG"
    Then I should not see pattern "\(abc\|def\|ghi\)"
    And the cursor should be at cell (2, 3)
    When I go to beginning of buffer
    And I type "dG"
    Then the buffer should be empty

  Scenario: delete backward char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz
    qux
    """
    And I type "dh"
    Then I should see pattern "^qu$"
    When I type "d3h"
    Then I should see pattern "-baz$"
    When I type "10dh"
    Then I should see pattern "^f$"

  Scenario: delete forward char
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz
    qux
    """
    And I go to beginning of buffer
    And I type "dl"
    Then I should see pattern "^oo "
    When I type "d5l"
    Then I should see pattern "r-baz$"
    When I type "7dl"
    Then I should see pattern "^ux$"

  Scenario: delete next line
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
    And I type "dj"
    Then I should not see pattern "\(foo\|bar\)"
    And the cursor should be at cell (1, 2)
    When I type "d2j"
    Then I should not see pattern "\(baz\|qux\|quux\)"
    And the cursor should be at cell (1, 4)
    When I go to line "2"
    And I type "3dj"
    Then I should not see pattern "[1-4]"
    And the cursor should be at cell (1, 4)

  Scenario: delete previous line
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
    And I type "dk"
    Then I should not see pattern "[12]"
    And the cursor should be at cell (8, 2)
    When I type "d3k"
    Then I should not see pattern "\([345]\|quux\)"
    And the cursor should be at cell (4, 3)
    When I go to line "3"
    When I type "2dk"
    Then I should not see pattern "\(foo\|bar\|baz\)"
    And the cursor should be at cell (1, 3)

  Scenario: delete forward word
    Given the buffer is empty
    When I insert:
    """
    foo   bar-baz
      qux quux
     123.456
    """
    And I go to beginning of buffer
    And I type "dw"
    Then I should not see pattern "^foo "
    And I should see pattern "^bar-"
    When I type "d2w"
    Then I should not see pattern "^bar-baz$"
    When I type "3dw"
    Then I should see pattern "^456$"

  Scenario: delete forward Word
    Given the buffer is empty
    When I insert:
    """
    foo   bar-baz
      qux quux
     123.456
    """
    And I go to beginning of buffer
    And I type "dW"
    Then I should not see pattern "^foo "
    And I should see pattern "^bar-"
    When I type "d2W"
    Then I should not see pattern "qux"
    When I type "2dW"
    Then the buffer should be empty

  Scenario: delete end of line
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
    And I type "d$"
    Then I should see pattern "^foo$"
    When I go to beginning of buffer
    And I type "d2$"
    Then I should not see pattern "\(foo\|123\)"
    When I type "4d$"
    Then the buffer should be empty

  Scenario: delete end of line by D
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
    And I type "D"
    Then I should see pattern "^foo$"
    When I go to beginning of buffer
    And I type "2D"
    Then I should not see pattern "\(foo\|123\)"
    When I type "4D"
    Then the buffer should be empty
