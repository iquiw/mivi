Feature: Undo
  In order to undo edit
  As a user
  I want to restore previous editing state

  Scenario: undo and redo
    Given the buffer is empty
    When I type "ifoo"
    And I press "<escape>"
    And I type "u"
    Then the buffer should be empty
    When I type "u"
    Then I should see "foo"

    When I type "ifoo"
    And I press "<escape>"
    And I type "a bar"
    And I press "<escape>"
    And I type "u"
    Then I should see "foo"
    And I should not see "bar"
    When I type "u"
    And I should see "foo bar"

  Scenario: repeat undo and redo
    Given the buffer is empty
    When I type "ifoo"
    And I press "<escape>"
    And I type "a bar"
    And I press "<escape>"
    And I type "a baz"
    And I press "<escape>"
    And I type "u."
    Then I should see "foo"
    And I should not see "bar"

    Given the buffer is empty
    When I type "ifoo"
    And I press "<escape>"
    And I type "a bar"
    And I press "<escape>"
    And I type "a baz"
    And I press "<escape>"
    And I type "u.."
    Then the buffer should be empty

    Given the buffer is empty
    When I type "ifoo"
    And I press "<escape>"
    And I type "a bar"
    And I press "<escape>"
    And I type "a baz"
    And I press "<escape>"
    And I type "u..u.."
    Then I should see "foo bar baz"
