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

  Scenario: change forward char
    When I go to beginning of buffer
    And I type "cl"
    Then I should see pattern "^oo "
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
    foo bar
    baz
      baz bar
      foo
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
