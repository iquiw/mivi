Feature: Scroll
  In order to view buffer contents
  As a user
  I want to scroll window

  Scenario: C-d
    Given the buffer is empty
    When I insert "2" pages
    And I go to beginning of buffer
    And I press "C-d"
    Then the current top line should be half page down from beginning

    When I go to beginning of buffer
    And I start an action chain
    And I press "5"
    And I press "C-d"
    And I execute the action chain
    Then the current top line should be "5" down from beginning

  Scenario: C-u
    Given the buffer is empty
    When I insert "2" pages
    And I go to end of buffer
    And I recenter on line "-1"
    And I press "C-u"
    Then the current bottom line should be half page up from end

  @fail
  Scenario: C-u with prefix
    When I go to end of buffer
    And I recenter on line "-1"
    And I start an action chain
    And I press "12"
    And I press "C-u"
    And I execute the action chain
    Then the current bottom line should be "12" up from end

  Scenario: C-f
    Given the buffer is empty
    When I insert "3" pages
    And I go to beginning of buffer
    And I press "C-f"
    Then the current top line should be "1" page down from beginning

    When I go to beginning of buffer
    And I start an action chain
    And I press "2"
    And I press "C-f"
    And I execute the action chain
    Then the current top line should be "2" pages down from beginning

  Scenario: C-b
    Given the buffer is empty
    When I insert "4" pages
    When I go to end of buffer
    And I recenter on line "-1"
    And I press "C-b"
    Then the current bottom line should be "1" page up from end

    When I go to end of buffer
    And I recenter on line "-1"
    And I start an action chain
    And I press "3"
    And I press "C-b"
    And I execute the action chain
    Then the current bottom line should be "3" pages up from end
