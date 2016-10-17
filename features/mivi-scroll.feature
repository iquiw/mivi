Feature: Scroll
  In order to view buffer contents
  As a user
  I want to scroll window

  Scenario: C-d
    Given the buffer is empty
    When I insert "2" pages
    And I go to beginning of buffer
    And I press "C-d"
    Then the current line should be half page down from beginning

    When I go to beginning of buffer
    And I start an action chain
    And I press "5"
    And I press "C-d"
    And I execute the action chain
    Then the current line should be "6" down from beginning

  Scenario: C-u
    Given the buffer is empty
    When I insert "2" pages
    And I go to end of buffer
    And I recenter on line "-1"
    And I press "C-u"
    Then the current line should be half page up from end

    When I go to end of buffer
    And I recenter on line "-1"
    And I start an action chain
    And I press "12"
    And I press "C-u"
    And I execute the action chain
    Then the current line should be "12" up from end
