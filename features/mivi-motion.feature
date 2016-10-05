Feature: Motion
  In order to edit text in command mode
  As a user
  I want to move cursor

  Scenario: h
    Given the buffer is empty
    When I insert:
    """
    1234567890 1234 5678
    abcdefghij

    foo bar baz
    """
    When I go to point "3"
    And I type "h"
    Then the cursor should be at cell (1, 1)
    When I type "h"
    Then the cursor should be at cell (1, 0)
    When I go to line "3"
    When I type "h"
    Then the cursor should be at cell (2, 10)

  Scenario: j
    When I go to beginning of buffer
    And I type "j"
    Then the cursor should be at cell (2, 0)
    When I go to end of line
    And I type "j"
    Then the cursor should be at cell (3, 0)
    When I go to cell (2, 10)
    And I type "jj"
    Then the cursor should be at cell (4, 10)

  Scenario: k
    When I go to end of buffer
    And I type "k"
    Then the cursor should be at cell (3, 0)
    When I go to cell (2, 5)
    And I type "k"
    Then the cursor should be at cell (1, 5)
    When I go to cell (4, 7)
    And I type "kk"
    Then the cursor should be at cell (2, 7)

  Scenario: l
    When I go to beginning of buffer
    And I type "l"
    Then the cursor should be at cell (1, 1)
    When I go to cell (2, 9)
    And I type "l"
    Then the cursor should be at cell (2, 10)
    When I type "l"
    Then the cursor should be at cell (3, 0)
