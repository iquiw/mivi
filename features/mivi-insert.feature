Feature Insert
  In order to insert text in normal way
  As a user
  I want to switch in insert state

  Scenario: insert
    Given the buffer is empty
    When I type "i"
    Then the mivi state should be "insert"

    When I type "foo bar baz"
    And I press "<escape>"
    Then the mivi state should be "command"
    And I should see pattern "^foo bar baz$"
    And the cursor should be at cell (1, 10)

    When I go to cell (1, 5)
    And I type "i"
    Then the mivi state should be "insert"
    And the cursor should be at cell (1, 5)
