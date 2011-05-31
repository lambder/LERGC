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
      :see-also [["http://en.wikipedia.org/wiki/Parsing_expression_grammar"]
                 ["http://www.itu.dk/~carsten/courses/f02/handouts/MonadicParserCombinators.pdf"]
                 ["http://mvanier.livejournal.com/5103.html"]
                 ["http://pdos.csail.mit.edu/~baford/packrat/thesis/thesis.pdf"]
                 ["http://pdos.csail.mit.edu/papers/packrat-parsing:icfp02.pdf"]]
      :doc "
LERGC - is Lazy Evaluation Recursive Grammar Combinators for Clojure.
It is inspired by Monadic Parser Combinators (Hutton & Meijer 1996; http://www.itu.dk/~carsten/courses/f02/handouts/MonadicParserCombinators.pdf)
which shows how to buld parser combinators in Gofer (statically typed, lazy functional language).
The LERGC implementation differs from the Gofer one since Clojure is not lazy. Laziness was realized by changing the monadic value signature
in state monad from 'S -> [V, S]' to 'unit -> S -> [V, S]', so monadic values combinators execute them only as required.
Another addition is that of representing failed computation (parser not matching). Rather than representing the failure as universal zero, it is represented
 as variant in 'Either' data type, here in Clojure implemented as closures (see: Error-handling Monads http://mvanier.livejournal.com/5103.html).

The state is not just simple string of characters but a structure denoting coordinates of characters consumption position (column/row),
  so the input is on the form of {:i INPUT_STRING, :c COLUMN_INT, :r ROW_INT} for convenience if we feed the {:i SOME_STRING} the default column/row will be set to 0:0.

 Roadmap:

 1.) add memoization to reduce the cost from O(n^p) to O(n) - Packard parser (Practical Linear-Time Algorithm with Backtracking http://pdos.csail.mit.edu/~baford/packrat/thesis/thesis.pdf)
 2.) implement the performance benchmarks showing the benefits of memoization.
    "}
  lergc.lergc
  (:use [clojure.contrib.monads]))

;; Either datatype  (http://mvanier.livejournal.com/5103.html)
(defn left? [obj]
  (obj :left?))

(defn right? [obj]
  (obj :right?))

(defn value [obj]
  (obj :value))
;; left constructor
(defn left [value]
  (fn [query]
    (cond
      (= query :left?) true
      (= query :value) value
      :else nil
      )
    )
  )
;; right constructor
(defn right [value]
  (fn [query]
    (cond
      (= query :right?) true
      (= query :value) value
      :else nil
      )
    ))
;; ~ Either datatype

(def correct right)
(def failure left)
(def correct? right?)
(def failure? left?)


(defmonad parser-m
  #^{:doc "Monad describing parsing computations. The monadic values have the
   structure (fn [] (fn [{:i input-string}] [result {:i remaining-input-string}])).
   Namely zero-parameter lambda returning parsing function.
   Parsing function takes input (state) and returns pair being parsing result and partially consumed input (new state).
   The wrapping lambda is used to make parsers lazy.
   Result is of Either type (failure|correct). Correct - for successful match. Failure - for no match.
   the (value result) retrieves the parsing direct result or failure details."}
  [
    m-result (fn m-result-parser [v]
    #(fn [input] [(correct v) input]))

    m-bind (fn m-bind-parser [mv f]
    #(fn [input]
      (let [[v consumed-input] ((mv) input)]
        (if (failure? v)
          [v input]
          (((f (value v))) consumed-input)))))

    m-zero #(fn m-zero-parser [s] [(failure :zero) s])

    m-plus (fn m-plus-parser [& parsers] ; these monad values are parsers
    (fn []
      (fn [input]
        (let [value-new-input
              (first
                (drop-while #(failure? (first %))
                  (map #((%) input) parsers)))] ; this is lazy map. It doesn't iterate over all parsers but only to the first successful one.
          (if (nil? value-new-input)
            [(failure "no parser matched") input]
            value-new-input
            )))))
    ])

(defmacro for-parsers
  "Parsers comprehension."
  ([steps expr]
    `(domonad parser-m ~steps ~expr)))


; parser matching any single character
(def any-char
  #(fn [state]
    ; i-input, c-column, r-row
    (let [{[head & rest] :i c :c r :r, :as state} (merge {:c 0 :r 0} state)]
      (cond
        (nil? head) [(failure "eof"), state]
        (= \newline head)
        [(correct head), {:i rest, :c 0, :r (inc r)}]
        :else
        [(correct head), {:i rest, :c (inc c), :r r}]))))

; parser matching any single character satisfying given predicate
(defn char-test [pred]
  #(fn [state]
    (let [[v next-state :as result] ((any-char) state)]
      (if (failure? v)
        result
        (if (pred (value v))
          result
          [(failure (format "%s not expected here" (value v))), state]
          )
        ))))

; parser matching any single character equal to given one [c]
(defn is-char [c]
  (char-test (partial = c)))


; parser matching string of given characters
(defn match-string [[h & r]]

  (if (nil? h)
    (for-parsers
      []
      "")
    (for-parsers
      [c (is-char h)
       cs (match-string r)]
      (str c cs))))


; parser combinator, crates parser that matches exactly as the underlying one or in case the underlying one does not match, then it matches any input, returning nil as a value.
(defn optional [parser]
  (with-monad parser-m
    (m-plus parser (m-result nil))))

; parser combinator, crates parser that matches whenever first one from of underlying parsers matches the input.
(def match-one
  (with-monad parser-m m-plus))

; parser combinator, crates parser that matches if all underlying parsers matches subsequently the input.
(defn match-all [& parsers]
  (with-monad parser-m
    (m-bind (m-seq parsers)
      (fn [x]
        (m-result
          x)))))

; parser combinator, crates parser that matches one or more times what the underlying parser matches subsequently.
(defn one-or-more [parser]
  (for-parsers
    [a parser
     as (optional (one-or-more parser))] ; this is not reccurency !!! we are acting lazyliy here
    (if (nil? as)
      (list a)
      (list* a as)
      )))

; parser combinator, crates parser that matches zero or more times what the underlying parser matches subsequently.
(defn none-or-more [parser]
  (optional (one-or-more parser)))

; parser combinator, crates parser that matches as any of the underlying parsers matches. First such match discards any subsequent parsers.
(defn one-of [target-strn]
  (let [str-chars (into #{} target-strn)]
    (char-test #(contains? str-chars %))))


; set of basic parsers
(def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def whitespace (one-of " \t\n\r"))
(def digit (one-of "0123456789"))
(def hexdigit (one-of "0123456789abcdefghABCDEFGH"))

