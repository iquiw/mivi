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
    Then I should see pattern "^foo bar baz qux  quux$"
    And the cursor should be at cell (1, 16)

    Given the buffer is empty
    When I insert:
    """
    foo

    bar
    """
    And I go to line "2"
    And I type "J"
    Then I should see:
    """
    foo
    bar
    """
    And the cursor should be at cell (2, 0)

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
    When I type "r7"
    Then I should see pattern "^7oo$"
    When I go to cell (1, 1)
    And I type "2rP"
    Then I should see pattern "^7PP$"
    And the cursor should be at cell (1, 1)

  Scenario: updown case
    Given the buffer is empty
    When I insert:
    """
    foo BAR BaZ
    """
    And I go to beginning of buffer
    And I type "~"
    Then I should see "Foo BAR BaZ"
    And I type "2~"
    Then I should see "FOO BAR BaZ"
    And I type "4~"
    Then I should see "FOO bar BaZ"
    And I type "4~"
    Then I should see "FOO bar bAz"

  Scenario: repeat the last substitute
    Given the buffer is empty
    When I insert:
    """
    foo
      bar baz
    qux quux
    """
    And I go to beginning of buffer
    And I type ":s/[a-z]\{3\}/quux/"
    And I go to line "2"
    And I type "&"
    Then I should see:
    """
    quux
      quux baz
    qux quux
    """

  Scenario: kill searched text
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    foo
    """
    And I go to beginning of buffer
    And I start an action chain
    And I press "C-a"
    And I press "C-w"
    And I execute the action chain
    Then I should see:
    """
    foo
    bar

    """

  Scenario: copy searched text
    Given the buffer is empty
    When I insert:
    """
    bar
    baz
    foo bar
    """
    And I go to beginning of buffer
    And I start an action chain
    And I press "C-a"
    And I press "M-w"
    And I execute the action chain
    Then I should see:
    """
    bar
    baz
    foo bar
    """
    And the current kill-ring should be "bar"
