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

(ert-deftest test-company-tern-property-marker-allow-own-properties ()
  (let ((candidate "property"))
    (put-text-property 0 1 'isProperty t candidate)
    (put-text-property 0 1 'depth 0 candidate)
    (should (company-tern-own-property-p candidate))))

(ert-deftest test-company-tern-property-marker-abort-prototype-properties ()
  (let ((candidate "property"))
    (put-text-property 0 1 'isProperty t candidate)
    (put-text-property 0 1 'depth 1 candidate)
    (should-not (company-tern-own-property-p candidate))))

(ert-deftest test-company-tern-property-marker-ignore-keyword ()
  (let ((candidate "keyword"))
    (put-text-property 0 1 'isKeyword t candidate)
    (put-text-property 0 1 'depth 0 candidate)
    (should-not (company-tern-own-property-p candidate))))

(provide 'company-tern-test)

;;; company-tern-test.el ends here
