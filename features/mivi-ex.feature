Feature: Ex command
  In order to manipulate text by line-oriented commands
  As a user
  I want to run ex command

  Scenario: ert tests
    When I run ert tests
    Then All ert tests should pass

  Scenario: go to the specified line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":5"
    Then the current line should be "5"

  Scenario: go to end of the specified line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":5,9"
    Then the current line should be "9"

  Scenario: delete the current line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "2"
    And I type ":d"
    Then I should not see "2"

  Scenario: delete the specified line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":5d"
    Then I should not see "5"

  Scenario: delete the specified line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":7,$d"
    Then I should not see pattern "\([789]\|10\)"

  Scenario: delete the relative line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "5"
    And I type ":-2,.+4d"
    Then I should not see pattern "\([3-9]\)"

  Scenario: delete the marked line range
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "3"
    And I type "ma"
    And I go to line "8"
    And I type "mb"
    And I type ":'a,'bd"
    Then I should not see pattern "[3-8]"

  Scenario: delete the search pattern range
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
       baz
    qux
    quux
    """
    And I go to line "1"
    And I type ":/ *bar/,/^qux/d"
    Then I should see:
    """
    foo
    quux
    """

  Scenario: delete the backward search pattern range
    Given the buffer is empty
    When I insert:
    """
    foo
      bar
       baz
    qux
    quux
    """
    And I go to line "5"
    And I type ":?ba.?,?qu*x?d"
    Then I should see:
    """
    foo
      bar
    quux
    """

  Scenario: delete the backward and forward search pattern range
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 foo
    5 bar
    """
    And I go to line "3"
    And I type ":?bar?,/foo/d"
    Then I should see:
    """
    1 foo
    5 bar
    """

  Scenario: substitute the current line
    Given the buffer is empty
    When I insert:
    """
    abc
    foo bar baz
    def
    """
    And I go to line "2"
    And I type ":s/foo/qux/"
    Then I should see "qux bar baz"
    And the current line should be "2"

  Scenario: substitute the specified line
    Given the buffer is empty
    When I insert:
    """
    abc
    foo bar baz
       123def
    def
    """
    And I type ":3s/[0-9]+/abc/"
    Then I should see "   abcdef"
    And the current line should be "3"

  Scenario: substitute with empty replace
    Given the buffer is empty
    When I insert "abc123def"
    And I type ":s/[0-9]+//"
    Then I should see "abcdef"

  Scenario: substitute the specified line range
    Given the buffer is empty
    When I insert:
    """
    abcabc
    foo bar baz
       abcdef
    def
    """
    And I type ":1,$s/abc/123"
    Then I should see:
    """
    123abc
    foo bar baz
       123def
    def
    """
    And the current line should be "3"

  Scenario: substitute with global flag
    Given the buffer is empty
    When I insert:
    """
    abcabc
    foo bar baz
       abcdef
    Adef
    """
    And I type ":1,$s/a../aa/g"
    Then I should see:
    """
    aaaa
    foo baabaz
       aadef
    Adef
    """
    And the current line should be "3"

  Scenario: substitute with matched string
    Given the buffer is empty
    When I insert:
    """

    def foo()

    """
    And I type ":2s/foo()/\&:/"
    Then I should see:
    """

    def foo():

    """

  Scenario: substitute with grouped matched string
    Given the buffer is empty
    When I insert:
    """
    def foo():

    def bar():
    """
    And I type ":1,$s/def \(.*\):/fn \1 {/"
    Then I should see:
    """
    fn foo() {

    fn bar() {
    """
