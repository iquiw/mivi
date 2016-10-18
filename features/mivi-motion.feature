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
    And I go to point "3"
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

  Scenario: find
    Given the buffer is empty
    When I insert:
    """
    abc def ghi

    012 345 678
    foo!bar!baz
    """
    And I go to beginning of buffer
    And I type "fh"
    Then the cursor should be at cell (1, 9)
    When I type "f2"
    Then the cursor should be at cell (3, 2)
    When I type "2f "
    Then the cursor should be at cell (3, 7)
    When I type "fofo"
    Then the cursor should be at cell (4, 2)
    When I type "f@"
    Then the cursor should be at cell (4, 2)
    When I go to end of buffer
    And I type "f@"
    Then the cursor should be at cell (4, 11)

  Scenario: Find
    When I go to end of buffer
    And I type "Fr"
    Then the cursor should be at cell (4, 6)
    When I type "Fi"
    Then the cursor should be at cell (1, 10)
    When I go to end of buffer
    And I type "FaFa"
    Then the cursor should be at cell (4, 5)
    When I go to end of buffer
    And I type "2Fa"
    Then the cursor should be at cell (4, 5)
    When I go to beginning of buffer
    When I type "F@"
    Then the cursor should be at cell (1, 0)

  Scenario: repeat find
    When I go to beginning of buffer
    And I type "f "
    And I type ";"
    Then the cursor should be at cell (1, 7)
    And I type "2;"
    Then the cursor should be at cell (3, 7)
    When I type "F!"
    And I go to end of buffer
    And I type "2;"
    Then the cursor should be at cell (4, 3)

  Scenario: repeat find opposite
    When I go to beginning of buffer
    And I type "3f "
    And I type ","
    Then the cursor should be at cell (1, 7)
    And I type "2,"
    Then the cursor should be at cell (3, 7)
    When I go to end of buffer
    When I type "3Fa"
    And I type "2,"
    Then the cursor should be at cell (4, 9)
