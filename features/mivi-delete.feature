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
