(require 'f)

(defvar mivi-support-path
  (f-dirname load-file-name))

(defvar mivi-features-path
  (f-parent mivi-support-path))

(defvar mivi-root-path
  (f-parent mivi-features-path))

(add-to-list 'load-path mivi-root-path)

(require 'mivi)
(require 'espuds)
(require 'ert)

(Setup
 (mivi-mode 1))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
