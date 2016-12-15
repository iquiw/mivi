Feature: Copy
  In order to edit text in command mode
  As a user
  I want to copy text

  Scenario: copy backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux
    qu_ux
    """
    And I type "yb"
    Then the current kill-ring should be "qu_ux"
    And the cursor should be at cell (2, 5)
    And I should see pattern "^qu_ux$"
    And the mivi state should be "command"
    When I type "2yb"
    Then the current kill-ring should be:
    """
    qux
    qu_ux
    """
    And the mivi state should be "command"
    When I type "y3b"
    Then the current kill-ring should be:
    """
    baz qux
    qu_ux
    """
    And the mivi state should be "command"

  Scenario: copy Backward word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
    """
    And I type "yB"
    Then the current kill-ring should be "qux-quux"
    And the mivi state should be "command"
    When I type "2yB"
    Then the current kill-ring should be "bar-baz qux-quux"
    And the mivi state should be "command"
    When I type "y3B"
    Then the current kill-ring should be "foo bar-baz qux-quux"
    And the mivi state should be "command"

  Scenario: copy end of word
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz qux-quux
       12345 6789
    """
    And I go to beginning of buffer
    And I type "ye"
    Then the current kill-ring should be "foo"
    And the mivi state should be "command"
    And the cursor should be at cell (1, 0)

  Scenario: copy End of word
    When I type "2yE"
    Then the current kill-ring should be "foo bar-baz"
    And the mivi state should be "command"
    And the cursor should be at cell (1, 0)
