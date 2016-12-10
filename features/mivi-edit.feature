Feature: Motion
  In order to edit text in command mode
  As a user
  I want to perform various edit commands

  Scenario: join
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
       baz
          qux  
    quux
    """
    And I go to beginning of buffer
    And I type "J"
    Then I should see pattern "^foo bar$"
    And the cursor should be at cell (1, 3)
    When I type "2J"
    Then I should see pattern "^foo bar baz$"
    And the cursor should be at cell (1, 7)
    When I type "3J"
    Then I should see pattern "^foo bar baz qux quux$"
    And the cursor should be at cell (1, 15)

  Scenario: replace char
    Given the buffer is empty
    When I insert:
    """
    foo
    """
    And I go to beginning of buffer
    And I type "rg"
    Then I should see pattern "^goo$"
    And the cursor should be at cell (1, 0)
    When I go to cell (1, 1)
    And I type "2rP"
    Then I should see pattern "^gPP$"
    And the cursor should be at cell (1, 1)
