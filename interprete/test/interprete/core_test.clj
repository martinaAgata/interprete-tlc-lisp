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
  (testing "controlar-aridad devuelve error al recibir demasiados argumentos"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
  )
)

(deftest controlar-aridad-many-arguments
  (testing "controlar-aridad devuelve error al recibir insuficientes argumentos"
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

; tests de error?

(deftest error?-first-element-is-error
  (testing "error? devuelve true al recibir una lista con primer elemento \"*error*\""
    (is (= true (error? '(*error* too-few-args))))
  )
)

(deftest error?-first-element-of-2-values-list-is-error
  (testing "error? devuelve true al recibir una lista con varios elementos donde el primero es \"*error*\""
    (is (= true (error? '(*error* too-many-args))))
  )
)

(deftest error?-first-element-of-2-values-list-is-error-in-uppercase
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"ERROR\""
    (is (= true (error? '(*ERROR* too-few-args))))
  )
)

(deftest error?-first-element-of-2-values-list-is-error-with-first-letter-on-uppercase
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"Error\""
    (is (= true (error? '(*Error* too-few-args))))
  )
)

(deftest error?-list-only-element-is-error
  (testing "error? devuelve true al recibir una lista con un único elemento \"*error*\""
    (is (= true (error? '(*error*))))
  )
)

(deftest error?-list-only-element-is-not-error
  (testing "error? devuelve false al recibir una lista con un único elemento distinto de \"*error*\" (o alguna de sus variaciones en distinto case)"
    (is (= false (error? (list 'too-few-args))))
  )
)

(deftest error?-error-is-not-inside-a-list
  (testing "error? devuelve false al no recibir el error dentro de una lista"
    (is (= false (error? '*error*)))
  )
)

(deftest error?-empty-list
  (testing "error? devuelve false al recibir una lista sin mensaje de error"
    (is (= false (error? ())))
  )
)

(deftest error?-nil
  (testing "error? devuelve false al recibir nil"
    (is (= false (error? ())))
  )
)

; tests de revisar-fnc

(deftest revisar-fnc-returns-error-list
  (testing "revisar-fnc devuelve el listado con mensaje de error recibido"
    (is (= (list '*error* 'too-few-args) (revisar-fnc '(*error* too-few-args))))
  )
)

(deftest revisar-fnc-returns-nil-when-list-is-not-an-error-list
  (testing "revisar-fnc devuelve nil al recibir un listado que no es un mensaje de error"
    (is (= nil (revisar-fnc '(too-few-args))))
  )
)

(deftest revisar-fnc-returns-nil-when-parameter-is-not-a-list
  (testing "revisar-fnc devuelve nil al no recibir una lista como parámetro"
    (is (= nil (revisar-fnc '*error*)))
  )
)

(deftest revisar-fnc-returns-nil-when-receiving-nil
  (testing "revisar-fnc devuelve nil al recibir nil"
    (is (= nil (revisar-fnc nil)))
  )
)

(deftest revisar-fnc-returns-nil-when-receiving-an-empty-list
  (testing "revisar-fnc devuelve nil al recibir una lista vacía"
    (is (= nil (revisar-fnc ())))
  )
)

; tests de revisar-lae

(deftest revisar-lae-returns-nil-when-list-has-no-errors
  (testing "revisar-lae devuelve nil si el listado no contiene ningún error"
    (is (= nil (revisar-lae '(1 2 3))))
  )
)

(deftest revisar-lae-returns-nil-when-receiving-nil
  (testing "revisar-lae devuelve nil cuando recibe nil"
    (is (= nil (revisar-lae nil)))
  )
)

(deftest revisar-lae-returns-nil-when-receiving-an-empty-list
  (testing "revisar-lae devuelve nil cuando recibe una lista vacía"
    (is (= nil (revisar-lae ())))
  )
)

(deftest revisar-lae-returns-error-when-list-has-an-error
  (testing "revisar-lae devuelve el único error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
  )
)

(deftest revisar-lae-returns-first-error-when-list-has-many-errors
  (testing "revisar-lae devuelve el primer error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))
  )
)

; tests de actualizar-amb

(deftest actualizar-amb-loads-new-key-value
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo par clave-valor"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
  )
)

(deftest actualizar-amb-replaces-preexistent-key-value
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo valor de la clave preexistente"
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
  )
)

(deftest actualizar-amb-does-not-modify-the-ambient-when-value-is-error
  (testing "actualizar-amb devuelve el ambiente sin modificaciones cuando el nuevo valor para una clave preexistente es un error"
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))))
  )
)

(deftest actualizar-amb-creates-new-ambient-when-receiving-empty-list
  (testing "actualizar-amb devuelve un ambiente que sólo posee los pares clave-valor que se le hayan indicado"
    (is (= '(b 7) (actualizar-amb () 'b 7)))
  )
)

; tests de buscar

(deftest buscar-existent-key-returns-value
  (testing "buscar una clave existente devuelve un valor asociado"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
  )
)

(deftest buscar-non-existent-key-returns-value
  (testing "buscar una clave inexistente devuelve error unbound-symbol"
    (is (= (list '*error* 'unbound-symbol 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)
