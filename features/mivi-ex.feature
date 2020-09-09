Feature: Ex command
  In order to manipulate text by line-oriented commands
  As a user
  I want to run ex command

  Scenario: ert tests
    Given the buffer is empty
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
    When I insert:
    """
       foo
       bar
       baz
       qux
       quux
    """
    And I go to word "bar"
    And I type "ma"
    And I go to word "qux"
    And I type "mb"
    And I type ":'a,'bd"
    Then I should see:
    """
       foo
       quux
    """

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

  Scenario: delete at end of buffer
    Given the buffer is empty
    When I insert:
    """
    1

    """
    And I go to cell (2, 0)
    And I type ":d"
    Then I should see:
    """
    1

    """
    And the cursor should be at cell (1, 0)

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
    And I should see message "Replaced 1 occurrence"

  Scenario: substitute with empty replace
    Given the buffer is empty
    When I insert "abc123def"
    And I type ":s/[0-9]+//"
    Then I should see "abcdef"

  Scenario: substitute with empty match
    Given the buffer is empty
    When I insert:
    """
    f o  oo

    """
    And I go to line "1"
    And I type ":s/ */a/g"
    Then I should see pattern "^afaoaoao$"

  Scenario: substitute the specified line range with empty match
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I go to line "1"
    And I type ":1,$s/o*/z/g"
    Then I should see:
    """
    zfz
    zbzazr
    zbzazz
    """

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
    And I should see message "Replaced 2 occurrences"

  Scenario: substitute the specified line range when buffer is growing
    Given the buffer is empty
    When I insert:
    """
    foo
    foo
    foo
    foo
    """
    And I type ":2,3s/foo/barbazqux/"
    Then I should see:
    """
    foo
    barbazqux
    barbazqux
    foo
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

  Scenario: substitute the search pattern range with pattern omitted
    Given the buffer is empty
    When I insert:
    """
    foo
    foo bar baz
       bar
    qux
    """
    And I go to line "1"
    And I type ":/bar/s//abc/"
    Then I should see:
    """
    foo
    foo abc baz
       bar
    qux
    """
    And the current line should be "2"

  Scenario: substitute line into multiple lines
    Given the buffer is empty
    When I insert:
    """
    foo,bar,baz
    """
    And I start an action chain
    And I type ":s/,/"
    And I press "C-q"
    And I press "C-j"
    And I type "/g"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    foo
    bar
    baz
    """

  Scenario: substitute backslash
    Given the buffer is empty
    When I insert:
    """
    \foo\bar\baz
    """
    And I type ":s,\\,/,g"
    Then I should see:
    """
    /foo/bar/baz
    """

  Scenario: repeat substitute
    Given the buffer is empty
    When I insert:
    """
    foo
    foo
    """
    And I go to line "1"
    And I type ":s/foo/bar/"
    And I go to line "2"
    And I type ":&"
    Then I should see:
    """
    bar
    bar
    """
    And I should see message "Replaced 1 occurrence"

  Scenario: repeat substitute with global flag
    Given the buffer is empty
    When I insert:
    """
    foo
    foo foo foo
    """
    And I go to line "1"
    And I type ":s/foo/bar/"
    And I go to line "2"
    And I type ":&g"
    Then I should see:
    """
    bar
    bar bar bar
    """
    And I should see message "Replaced 3 occurrences"

  Scenario: copy the current line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I go to line "2"
    And I type ":y"
    Then the current kill-ring should be:
    """
    2

    """

  Scenario: copy the specified line
    Given the buffer is empty
    When I insert numbers per line to "10"
    And I type ":5y"
    Then the current kill-ring should be:
    """
    5

    """

  Scenario: copy the specified line range
    Given the buffer is empty
    When I insert:
    """
    foo
    bar
    baz
    """
    And I type ":1,$y"
    Then the current kill-ring should be:
    """
    foo
    bar
    baz

    """

  Scenario: global delete
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    """
    And I type ":g/[23]/d"
    Then I should see:
    """
    1 foo
    4 qux
    """

  Scenario: global delete with range
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    5 quux
    """
    And I type ":2,4g/qu/d"
    Then I should see:
    """
    1 foo
    2 bar
    3 baz
    5 quux
    """

  Scenario: global delete empty lines
    Given the buffer is empty
    When I insert:
    """
    1

    2

    3

    """
    And I type ":g/^$/d"
    Then I should see:
    """
    1
    2
    3

    """

  Scenario: global substitute
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    """
    And I type ":g/[14]/s/foo/foofoo/"
    Then I should see:
    """
    1 foofoo
    2 bar
    3 baz
    4 qux
    """

  Scenario: global substitute with substitute pattern omitted
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    """
    And I type ":g/\(foo\|baz\)/s//quux/"
    Then I should see:
    """
    1 quux
    2 bar
    3 quux
    4 qux
    """

  Scenario: inverse global delete
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    """
    And I type ":v/[23]/d"
    Then I should see:
    """
    2 bar
    3 baz
    """
    And I should not see pattern "\(foo\|qux\)"

  Scenario: inverse global delete with range
    Given the buffer is empty
    When I insert:
    """
    1 foo
     2 bar
      3 baz
       4 qux
        5 quux
    """
    And I type ":2,4v/ba/d"
    Then I should see:
    """
    1 foo
     2 bar
      3 baz
        5 quux
    """

  Scenario: inverse global delete non empty lines
    Given the buffer is empty
    When I insert:
    """
    1

    2

    3

    """
    And I type ":v/^$/d"
    Then I should not see pattern "[123]"
    And the cursor should be at cell (3, 0)

  Scenario: inverse global substitute
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    """
    And I type ":v/[14]/s/ba/baba/"
    Then I should see:
    """
    1 foo
    2 babar
    3 babaz
    4 qux
    """

  Scenario: inverse global substitute with range
    Given the buffer is empty
    When I insert:
    """
    1 foo
    2 bar
    3 baz
    4 qux
    5 quux
    """
    And I type ":2,4v/z/s/[0-9]/\&\&/"
    Then I should see:
    """
    1 foo
    22 bar
    3 baz
    44 qux
    5 quux
    """
