Feature: Ex command
  In order to manipulate text by line-oriented commands
  As a user
  I want to run ex command

  Scenario: ert tests
    When I run ert tests
    Then All ert tests should pass

  Scenario: delete the current line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "2"
    And I type ":d"
    And I press "RET"
    Then I should not see "2"

  Scenario: delete the specified line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":5d"
    And I press "RET"
    Then I should not see "5"

  Scenario: delete the specified line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":7,$d"
    And I press "RET"
    Then I should not see pattern "\([789]\|10\)"

  Scenario: delete the relative line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "5"
    And I type ":-2,.+4d"
    And I press "RET"
    Then I should not see pattern "\([3-9]\)"

  Scenario: delete the marked line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "3"
    And I type "ma"
    And I go to line "8"
    And I type "mb"
    And I type ":'a,'bd"
    And I press "RET"
    Then I should not see pattern "[3-8]"
