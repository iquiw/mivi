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
