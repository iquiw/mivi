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
