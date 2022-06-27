(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

; tests de controlar-aridad
(deftest controlar-aridad-success
  (testing "controlar-aridad devuelve longitud esperada"
    (is (= 3 (controlar-aridad '(a b c) 3)))
  )
)

(deftest controlar-aridad-too-many-arguments
  (testing "controlar-aridad recibió demasiados argumentos"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
  )
)

(deftest controlar-aridad-many-arguments
  (testing "controlar-aridad recibió insuficientes argumentos"
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)

; tests de igual?
(deftest igual?-equal-numbers
  (testing "true = (igual? 1 1)"
    (is (= true (igual? 1 1)))
  )
)

(deftest igual?-different-numbers
  (testing "false = (igual? 1 2)"
    (is (= false (igual? 1 2)))
  )
)

(deftest igual?-equal-literal-chars-same-case
  (testing "true = (igual? 'A 'A)"
    (is (= true (igual? 'A 'A)))
  )
)

(deftest igual?-equal-literal-chars-different-case
  (testing "true = (igual? 'a 'A)"
    (is (= true (igual? 'a 'A)))
  )
)

(deftest igual?-different-literal-chars
  (testing "false = (igual? 'a 'b)"
    (is (= false (igual? 'a 'b)))
  )
)

(deftest igual?-equal-lists-same-case
  (testing "true = (igual? '(a b c) '(a b c))"
    (is (= true (igual? '(a b c) '(a b c))))
  )
)

(deftest igual?-equal-lists-different-case
  (testing "true = (igual? '(a b c) '(A B C))"
    (is (= true (igual? '(a b c) '(A B C))))
  )
)

(deftest igual?-different-lists
  (testing "false = (igual? '(a b c) '(a b d))"
    (is (= false (igual? '(a b c) '(a b d))))
  )
)

(deftest igual?-nils
  (testing "true = (igual? nil nil)"
    (is (= true (igual? nil nil)))
  )
)

(deftest igual?-nil-against-literal-nil
  (testing "true = (igual? nil 'NIL)"
    (is (= true (igual? nil 'NIL)))
  )
)

(deftest igual?-nil-against-empty-list
  (testing "true = (igual? nil ())"
    (is (= true (igual? nil ())))
  )
)

(deftest igual?-empty-lists
  (testing "true = (igual? () ())"
    (is (= true (igual? () ())))
  )
)

(deftest igual?-empty-list-against-nil-list
  (testing "true = (igual? () '(nil))"
    (is (= true (igual? () '(nil))))
  )
)

(deftest igual?-equal-chars
  (testing "true = (igual? \"a\" \"a\")"
    (is (= true (igual? "a" "a")))
  )
)

(deftest igual?-different-case-chars
  (testing "false = (igual? \"a\" \"A\")"
    (is (= false (igual? "a" "A")))
  )
)

(deftest igual?-different-chars
  (testing "false = (igual? \"a\" \"b\")"
    (is (= false (igual? "a" "b")))
  )
)
