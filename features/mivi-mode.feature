Feature: Mode
  In order to edit text in mivi mode
  As a user
  I want to enable mivi in the specified modes

Scenario: Enable in prog-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on prog-mode
  Then mivi-mode should be enabled

Scenario: Enable in emacs-lisp-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on emacs-lisp-mode
  Then mivi-mode should be enabled

Scenario: Enable in conf-unix-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on conf-unix-mode
  Then mivi-mode should be enabled

Scenario: Enable in text-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on text-mode
  Then mivi-mode should be enabled

Scenario: Enable in fundamental-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on fundamental-mode
  Then mivi-mode should be enabled

Scenario: Disable in help-mode
  Given the buffer is empty
  When I disable C-u binding
  And I turn on help-mode
  Then mivi-mode should be disabled

Scenario: Disable by turn-off
  Given the buffer is empty
  When I disable C-u binding
  And I turn off minor mode mivi-mode
  Then mivi-mode should be disabled

Scenario: Global mivi mode off and on
  Given the buffer is empty
  When I disable C-u binding
  And I turn off minor mode mivi-global-mode
  And I turn on prog-mode
  Then mivi-mode should be disabled

  When I turn on mivi-global-mode
  And I turn on fundamental-mode
  Then mivi-mode should be enabled
