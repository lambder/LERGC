;; by Daniel Kwiecinski
;; last updated May, 2011

;; Copyright (c) Daniel Kwiecinski, 2011. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Daniel Kwiecinski"
      :doc "Test routines for LERGC"}
  lergc.test_lergc
  (:use [clojure.test :only (deftest is are run-tests)]
    [lergc.lergc]))


(deftest single-alpha
  (let [[parsing-result leftover-input]
        ((alpha) {:i "a9bcdef"})]
    (is (= (value parsing-result) \a))
    (is (correct? parsing-result))))

(deftest alpha-and-digit
  (let [[parsing-result leftover-input]
        (((match-all alpha digit)) {:i "a9bcdef"})
        ]
    (is (= (value parsing-result) '(\a \9)))
    (is (correct? parsing-result))))

(deftest alpha-and-digit-error
  (let [[parsing-result leftover-input]
        (((match-all alpha digit)) {:i "a+9bcdef"})]
    (is (= (value parsing-result) "+ not expected here"))
    (is (failure? parsing-result))))

(deftest one-or-more-alpha
  (let [[parsing-result leftover-input]
        (((match-all alpha digit)) {:i "abcdef"})]
    (is (failure? parsing-result))
    (is (= (value parsing-result) "b not expected here"))))

(deftest one-or-more-alpha2
  (let [[parsing-result leftover-input]
        (((match-all alpha digit)) {:i "a"})]
    (is (failure? parsing-result))
    (is (= (value parsing-result) "eof"))
    ))

(deftest none-or-more-alpha
  (let [[parsing-result leftover-input]
        (((none-or-more alpha)) {:i "a"})]
    (is (= (value parsing-result) '(\a)))
    (is (correct? parsing-result))))

(deftest none-or-more-alpha-empty-input
  (let [[parsing-result leftover-input]
        (((none-or-more alpha)) {:i ""})]
    (is (= (value parsing-result) nil))
    (is (correct? parsing-result))))

(deftest match-one-string
  (let [[parsing-result leftover-input]
        (((match-string "abrakadabra")) {:i "abrakadabra----"})]
    (is (= (:i leftover-input) '(\- \- \- \-))) ; leftover input are ---- characters
    (is (= (:c leftover-input) 11)) ; the input reading was left on 11th column
    (is (= (:r leftover-input) 0)) ; the input reading was left on zeroth row
    (is (= (value parsing-result) "abrakadabra"))
    (is (correct? parsing-result))))


(deftest match-repeated-string
  (let [[parsing-result leftover-input]
        (((one-or-more (match-string "abrakadabra+"))) {:i "abrakadabra+abrakadabra+abrakadabra+----"})]
    (is (= (:i leftover-input) '(\- \- \- \-))) ; leftover input are ---- characters
    (is (= (:c leftover-input) 36)) ; the input reading was left on 36th column
    (is (= (:r leftover-input) 0)) ; the input reading was left on zero row
    (is (= (value parsing-result) '("abrakadabra+" "abrakadabra+" "abrakadabra+")))
    (is (correct? parsing-result))))

(deftest match-repeated-string-with-new-lines
  (let [[parsing-result leftover-input]
        (((one-or-more (match-string "abrakadabra\n"))) {:i "abrakadabra\nabrakadabra\nabrakadabra\n----"})]
    (is (= (:i leftover-input) '(\- \- \- \-))) ; leftover input are ---- characters
    (is (= (:c leftover-input) 0)) ; the input reading was left on zero column
    (is (= (:r leftover-input) 3)) ; the input reading was left on 3rd row
    (is (= (value parsing-result) '("abrakadabra\n" "abrakadabra\n" "abrakadabra\n")))
    (is (correct? parsing-result))))