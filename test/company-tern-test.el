;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'company-tern)

(ert-deftest test-company-tern-function-p ()
  (should (company-tern-function-p "fn(i: number)")))

(ert-deftest test-company-tern-function-p-with-number ()
  (should (null (company-tern-function-p "number"))))

(provide 'company-tern-test)

;;; company-tern-test.el ends here
