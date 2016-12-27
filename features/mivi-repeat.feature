Feature: Undo
  In order to edit text quickly
  As a user
  I want to repeat previous command

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
    When I type "2."
    Then I should see pattern "^0$"
