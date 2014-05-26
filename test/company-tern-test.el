;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'company-tern)

(ert-deftest test-company-tern-function-p ()
  (should (company-tern-function-p "fn(i: number)")))

(ert-deftest test-company-tern-function-p-with-number ()
  (should (null (company-tern-function-p "number"))))

(ert-deftest test-company-tern-function-type ()
  (let (company-tooltip-align-annotations)
    (should (s-equals? (company-tern-function-type
                        "fn(test: fn(elt: ?, i: number) -> bool, context?: ?) -> bool")
                       "(test, context?)"))))

(ert-deftest test-company-tern-function-type-align-annotation ()
  (let ((company-tooltip-align-annotations t))
    (should (s-equals? (company-tern-function-type
                        "fn(test: fn(elt: ?, i: number) -> bool, context?: ?) -> bool")
                       "fn(test, context?)"))))

(ert-deftest test-company-tern-variable-type ()
  (let ((company-tooltip-align-annotations t))
    (should (s-equals? "number" (company-tern-variable-type "number")))))

(ert-deftest test-company-tern-variable-type-align-annotation ()
  (let (company-tooltip-align-annotations)
    (should (s-equals? " -> number" (company-tern-variable-type "number")))))

(provide 'company-tern-test)

;;; company-tern-test.el ends here
