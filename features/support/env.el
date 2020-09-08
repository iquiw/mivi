(require 'f)

(defvar mivi-support-path
  (f-dirname load-file-name))

(defvar mivi-features-path
  (f-parent mivi-support-path))

(defvar mivi-root-path
  (f-parent mivi-features-path))

(defvar mivi-ert-result)

(add-to-list 'load-path mivi-root-path)

(require 'undercover)
(undercover "mivi*.el"
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'mivi)
(require 'espuds)
(require 'ert)

(Setup
 (setq debug-on-error nil)
 (dolist (f (f-entries (expand-file-name "tests" mivi-root-path)))
   (load f nil t))
 (mivi-setup))

(Before
 ;; Before each scenario is run
 (lisp-interaction-mode)
 (read-only-mode -1)
 )

(After
 ;; After each scenario is run
 (define-key mivi-command-map (kbd "C-u") #'mivi-scroll-down)
 (mivi-command)
 )

(Teardown
 ;; After when everything has been run
 )
