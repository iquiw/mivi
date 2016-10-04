Feature: Motion
  In order to edit text in command mode
  As a user
  I want to move cursor

  Scenario: h
    Given the buffer is empty
    When I insert:
    """
    1234567890
    abcdefghij

    foo bar baz
    """
    When I go to point "3"
    And I type "h"
    Then the cursor should be at cell (1, 1)
    When I type "h"
    Then the cursor should be at cell (1, 0)

  Scenario: j
    When I go to beginning of buffer
    And I type "j"
    Then the cursor should be at cell (2, 0)
    When I go to end of line
    And I type "j"
    Then the cursor should be at cell (3, 0)
