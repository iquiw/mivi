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

  Scenario: Replace
    Given the buffer is empty
    When I insert:
    """
    foo bar
    """
    And I go to beginning of buffer
    And I type "Rbaz "
    Then the mivi state should be "insert"
    And I should see pattern "^baz bar$"
    When I press "<escape>"
    And I type "R qux quux"
    Then the mivi state should be "insert"
    And I should see pattern "^baz qux quux$"

  Scenario: substitute
    Given the buffer is empty
    When I insert:
    """
    foo bar
    """
    And I go to beginning of buffer
    And I type "squx"
    Then the mivi state should be "insert"
    And I should see pattern "^quxoo bar$"
    When I press "<escape>"
    And I place the cursor after "oo "
    And I type "3squux"
    Then the mivi state should be "insert"
    And I should see pattern "^quxoo quux$"

  Scenario: C-d
    Given the buffer is empty
    When I insert:
    """
    (defun foo ()
        
    """
    And I type "i"
    And I press "C-d"
    Then the cursor should be at cell (2, 2)
    When I press "C-d"
    Then the cursor should be at cell (2, 0)
    When I press "C-d"
    Then the cursor should be at cell (2, 0)

    When I type "    0"
    And I press "C-d"
    Then the cursor should be at cell (2, 0)

    When I go to cell (1, 13)
    And I press "RET"
    And I type "  ^"
    And I press "C-d"
    Then the cursor should be at cell (2, 2)

    When I go to word "foo"
    And I press "C-d"
    Then I should see pattern "^(defun oo"

    When I go to beginning of buffer
    And I press "C-d"
    Then I should see pattern "^defun oo"

  Scenario C-v
    Given the buffer is empty
    When I start an action chain
    And I press "C-v"
    And I press "C-a"
    And I execute the action chain
    Then I should see ""

  Scenario: C-w
    Given the buffer is empty
    When I insert:
    """
    foo bar-baz "<>"
    """
    When I type "i"
    And I press "C-w"
    Then I should see pattern "^foo bar-baz $"

    When I press "C-w"
    Then I should see pattern "^foo bar-$"

    When I press "C-w"
    Then I should see pattern "^foo bar$"
