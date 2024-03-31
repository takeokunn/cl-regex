(defpackage cl-regex/tests/main
  (:use :cl :rove))
(in-package :cl-regex/tests/main)

(deftest test-target-1
  (rove:testing "should (= 1 1) to be true"
    (rove:ok (= 1 1)))

  (testing ""
    (ok (equal (cl-regex:test) "foo"))))
