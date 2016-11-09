Feature Kill and Yank
  In order to edit text in command mode
  As a user
  I want to kill and yank text

  Scenario: x
    Given the buffer is empty
    When I insert:
    """
    foo bar baz qux
    123
    456
    789
    """
    And I go to beginning of buffer
    And I type "x"
    Then I should see pattern "^oo "

    When I go to line "2"
    And I go to end of line
    And I type "x"
    Then I should see pattern "^123456$"

    When I go to beginning of buffer
    And I type "10x"
    Then I should see pattern "^ qux$"

  Scenario: X
    Given the buffer is empty
    When I insert:
    """
    foo bar baz qux
    """
    And I go to end of line
    And I type "X"
    Then I should see pattern " qu$"

    When I type "3X"
    Then I should see pattern "baz$"
