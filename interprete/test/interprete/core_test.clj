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
  (testing "igual? devuelve true comparando dígitos iguales"
    (is (= true (igual? 1 1)))
  )
)

(deftest igual?-different-numbers
  (testing "igual? devuelve false comparando dígitos distintos"
    (is (= false (igual? 1 2)))
  )
)

(deftest igual?-equal-literal-chars-different-case
  (testing "igual? devuelve true comparando literales de iguales caracteres con distinto case"
    (is (= true (igual? 'a 'A)))
  )
)

(deftest igual?-different-literal-chars
  (testing "igual? devuelve false comparando caracteres distintos"
    (is (= false (igual? 'a 'b)))
  )
)

(deftest igual?-equal-lists-same-case
  (testing "igual? devuelve true comparando listas de caracteres iguales"
    (is (= true (igual? '(a b c) '(a b c))))
  )
)

(deftest igual?-equal-lists-different-case
  (testing "igual? devuelve true comparando listas de caracteres iguales pero con distinto case"
    (is (= true (igual? '(a b c) '(A B C))))
  )
)

(deftest igual?-different-lists
  (testing "igual? devuelve false comparando listas de caracteres distintas"
    (is (= false (igual? '(a b c) '(a b d))))
  )
)

(deftest igual?-nils
  (testing "igual? devuelve true comparando nils"
    (is (= true (igual? nil nil)))
  )
)

(deftest igual?-nil-against-literal-nil
  (testing "igual? devuelve true comparando nil contra el literal 'NIL"
    (is (= true (igual? nil 'NIL)))
  )
)

(deftest igual?-nil-against-empty-list
  (testing "igual? devuelve true comparando nil contra una lista vacía"
    (is (= true (igual? nil ())))
  )
)

(deftest igual?-empty-lists
  (testing "igual? devuelve true comparando listas vacías"
    (is (= true (igual? () ())))
  )
)

(deftest igual?-empty-list-against-nil-list
  (testing "igual? devuelve false comparando una lista vacía contra una lista cuyo único elemento es nil"
    (is (= false (igual? () '(nil))))
  )
)

(deftest igual?-equal-chars
  (testing "igual? devuelve true comparando strings iguales"
    (is (= true (igual? "a" "a")))
  )
)

(deftest igual?-different-case-chars
  (testing "igual? devuelve false comparando strings con distinto case"
    (is (= false (igual? "a" "A")))
  )
)

(deftest igual?-different-chars
  (testing "igual? devuelve false comparando strings distintos"
    (is (= false (igual? "a" "b")))
  )
)
