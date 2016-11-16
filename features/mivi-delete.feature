Feature: Delete
  In order to edit text in command mode
  As a user
  I want to delete text

  Scenario: db
    When the buffer is empty
    And I insert:
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

  Scenario: dB
    When the buffer is empty
    And I insert:
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
