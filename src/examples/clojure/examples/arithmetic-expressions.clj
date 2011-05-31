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
      :doc "Simple demonstration of construction of arithmetic expressions parser using LERGC. Run test by executing (clojure.test/run-tests 'examples.arithmetic-expressions) from REPL"}

  examples.arithmetic-expressions
  (:use [clojure.test :only (with-test is)]
    [lergc.lergc]))

(def integer
  (for-parsers
    [a (one-of "123456789")
     b (none-or-more digit)]
    (let [ds (cons a b)]
      (loop [
        [fd & rest :as digits] (map #(- (int %) 48) (reverse ds))
        mul 1
        acc 0]
        (if (empty? digits)
          acc
          (recur rest (* 10 mul) (+ acc (* mul fd))))))))



;  BNF of arithmetic expressions
; --------------------------------------------
;  Expr    ::= Product (('+' | '-') Product)*
;  Product ::= Factor (('*' | '/') Factor)*
;  Factor  ::= Integer | '(' Expr ')'

(with-test
  (def arithmetic
    (letfn
      [(expr []
         ((for-parsers
           [f product
            op-facts (none-or-more
             (match-all
               addop product))]
           (reduce (fn [acc [op fact]] (op acc fact)) f op-facts)
           )))

       (product []
         ((for-parsers
           [f factor
            op-facts (none-or-more
             (match-all
               mulop factor))]
           (reduce (fn [acc [op fact]] (op acc fact)) f op-facts)
           )))

       (mulop []
         ((match-one
           (for-parsers [op (is-char \*)] *)
           (for-parsers [op (is-char \/)] /))))

       (factor []
         ((match-one
           integer
           (for-parsers
             [p-open (is-char \()
              e expr
              p-close (is-char \))
              ]
             e))))

       (addop []
         ((match-one
           (for-parsers [op (is-char \+)] +)
           (for-parsers [op (is-char \-)] -))))
       ]
      (expr)))

  ;; test

  (let [[parsing-result leftover-input]
        (arithmetic {:i "2*3"})]
    (is (= (value parsing-result) 6)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "2+3"})]
    (is (= (value parsing-result) 5)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "(2+3)*2"})]
    (is (= (value parsing-result) 10)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "2+(3*2)"})]
    (is (= (value parsing-result) 8)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "2+3*2"})]
    (is (= (value parsing-result) 8)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "2*3+2"})]
    (is (= (value parsing-result) 8)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "1+2+3+4+5+6+7"})]
    (is (= (value parsing-result) 28)))

  (let [[parsing-result leftover-input]
        (arithmetic {:i "1*2*3*4*5*6*7"})]
    (is (= (value parsing-result) 5040)))
  )

