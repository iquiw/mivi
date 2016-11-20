Feature Insert
  In order to insert text in normal way
  As a user
  I want to switch in insert state

  Scenario: insert
    Given the buffer is empty
    When I type "i"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 0)

    When I type "foo bar baz"
    And I press "<escape>"
    Then the mivi state should be "command"
    And I should see pattern "^foo bar baz$"
    And the cursor should be at cell (1, 10)

    When I go to cell (1, 5)
    And I type "i"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 5)

  Scenario: Insert
    Given the buffer is empty
    When I type "I"
    Then the mivi state should be "insert"

    When I type "foo"
    And I press "<return>"
    And I type "   bar baz"
    And I press "<escape>"
    Then the mivi state should be "command"
    And I should see pattern "^   bar baz$"
    And the cursor should be at cell (2, 9)

    When I type "I"
    Then the mivi state should be "insert"
    And the cursor should be at cell (2, 3)

  Scenario: open
    Given the buffer is empty
    When I type "o"
    Then the mivi state should be "insert"
    And the cursor should be at cell (2, 0)

    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
    """
    And I press "<escape>"
    And I type "o"
    Then the mivi state should be "insert"
    And the cursor should be at cell (2, 2)

    When I insert "(when t"
    And I press "<escape>"
    And I start an action chain
    And I type "o"
    And I press "<escape>"
    And I execute the action chain
    Then the mivi state should be "command"
    And the cursor should be at cell (3, 0)

  Scenario: Open
    Given the buffer is empty
    When I type "O"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 0)

    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
      (if t
    
    """
    And I press "<escape>"
    And I type "O"
    Then the mivi state should be "insert"
    And the cursor should be at cell (3, 6)

    When I press "<escape>"
    And I start an action chain
    And I type "O"
    And I press "<escape>"
    And I execute the action chain
    Then the mivi state should be "command"
    And the cursor should be at cell (3, 0)

  Scenario: append
    Given the buffer is empty
    When I type "a"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 0)

    When I type "foo bar baz"
    And I press "<escape>"
    Then the mivi state should be "command"
    And I should see pattern "^foo bar baz$"
    And the cursor should be at cell (1, 10)

    When I type "a"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 11)

  Scenario: Append
    Given the buffer is empty
    When I type "A"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 0)

    When I type "foo bar baz"
    And I press "<escape>"
    Then the mivi state should be "command"
    And I should see pattern "^foo bar baz$"

    When I go to beginning of buffer
    And I type "A"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 11)
