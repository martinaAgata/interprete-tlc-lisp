(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

; tests de controlar-aridad
(deftest controlar-aridad-success
  (testing "controlar-aridad devuelve longitud esperada"
    (is (= 3 (controlar-aridad '(a b c) 3)))))

(deftest controlar-aridad-too-many-arguments
  (testing "controlar-aridad recibió demasiados argumentos"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))))

(deftest controlar-aridad-many-arguments
  (testing "controlar-aridad recibió insuficientes argumentos"
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))))
