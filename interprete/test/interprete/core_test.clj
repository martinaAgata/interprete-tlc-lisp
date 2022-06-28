(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

; tests de controlar-aridad

(deftest controlar-aridad-devuelve-longitud-esperada
  (testing "controlar-aridad devuelve longitud esperada"
    (is (= 3 (controlar-aridad '(a b c) 3)))
  )
)

(deftest controlar-aridad-recibe-demasiados-argumentos
  (testing "controlar-aridad devuelve error al recibir demasiados argumentos"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
  )
)

(deftest controlar-aridad-recibe-argumentos-insuficientes
  (testing "controlar-aridad devuelve error al recibir insuficientes argumentos"
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)

; tests de igual?

(deftest igual?-numeros-iguales
  (testing "igual? devuelve true comparando dígitos iguales"
    (is (= true (igual? 1 1)))
  )
)

(deftest igual?-numeros-distintos
  (testing "igual? devuelve false comparando dígitos distintos"
    (is (= false (igual? 1 2)))
  )
)

(deftest igual?-literales-iguales-en-distinto-case
  (testing "igual? devuelve true comparando literales de iguales caracteres con distinto case"
    (is (= true (igual? 'a 'A)))
  )
)

(deftest igual?-distintos-literales
  (testing "igual? devuelve false comparando caracteres distintos"
    (is (= false (igual? 'a 'b)))
  )
)

(deftest igual?-listas-iguales-en-mismo-case
  (testing "igual? devuelve true comparando listas de caracteres iguales"
    (is (= true (igual? '(a b c) '(a b c))))
  )
)

(deftest igual?-listas-iguales-en-distinto-case
  (testing "igual? devuelve true comparando listas de caracteres iguales pero con distinto case"
    (is (= true (igual? '(a b c) '(A B C))))
  )
)

(deftest igual?-listas distintas
  (testing "igual? devuelve false comparando listas de caracteres distintas"
    (is (= false (igual? '(a b c) '(a b d))))
  )
)

(deftest igual?-nils
  (testing "igual? devuelve true comparando nils"
    (is (= true (igual? nil nil)))
  )
)

(deftest igual?-nil-contra-nil-literal
  (testing "igual? devuelve true comparando nil contra el literal 'NIL"
    (is (= true (igual? nil 'NIL)))
  )
)

(deftest igual?-nil-contra-lista-vacia
  (testing "igual? devuelve true comparando nil contra una lista vacía"
    (is (= true (igual? nil ())))
  )
)

(deftest igual?-listas-vacias
  (testing "igual? devuelve true comparando listas vacías"
    (is (= true (igual? () ())))
  )
)

(deftest igual?-lista-vacia-contra-lista-con-nil
  (testing "igual? devuelve false comparando una lista vacía contra una lista cuyo único elemento es nil"
    (is (= false (igual? () '(nil))))
  )
)

(deftest igual?-mismos-strings
  (testing "igual? devuelve true comparando strings iguales"
    (is (= true (igual? "a" "a")))
  )
)

(deftest igual?-strings-distinto-case
  (testing "igual? devuelve false comparando strings con distinto case"
    (is (= false (igual? "a" "A")))
  )
)

(deftest igual?-distintos-strings
  (testing "igual? devuelve false comparando strings distintos"
    (is (= false (igual? "a" "b")))
  )
)

; tests de error?

(deftest error?-primer-elemento-de-lista-literal-es-error
  (testing "error? devuelve true al recibir una lista con primer elemento \"*error*\""
    (is (= true (error? '(*error* too-few-args))))
  )
)

(deftest error?-primer-elemento-de-lista-es-error
  (testing "error? devuelve true al recibir una lista con varios elementos donde el primero es \"*error*\""
    (is (= true (error? (list '*error* too-many-args))))
  )
)

(deftest error?-primer-elemento-de-lista-es-error-en-uppercase
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"ERROR\""
    (is (= true (error? '(*ERROR* too-few-args))))
  )
)

(deftest error?-primer-elemento-de-lista-es-error-con-solo-primera-letra-uppercase
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"Error\""
    (is (= true (error? '(*Error* too-few-args))))
  )
)

(deftest error?-lista-de-un-solo-elemento-error-es-error
  (testing "error? devuelve true al recibir una lista con un único elemento \"*error*\""
    (is (= true (error? '(*error*))))
  )
)

(deftest error?-lista-de-un-solo-elemento-distinto-de-error-no-es-error
  (testing "error? devuelve false al recibir una lista con un único elemento distinto de \"*error*\" (o alguna de sus variaciones en distinto case)"
    (is (= false (error? (list 'too-few-args))))
  )
)

(deftest error?-error-no-esta-dentro-de-una-lista
  (testing "error? devuelve false al no recibir el error dentro de una lista"
    (is (= false (error? '*error*)))
  )
)

(deftest error?-lista-vacia
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

(deftest revisar-fnc-devuelve-listado-de-error
  (testing "revisar-fnc devuelve el listado con mensaje de error recibido"
    (is (= (list '*error* 'too-few-args) (revisar-fnc '(*error* too-few-args))))
  )
)

(deftest revisar-fnc-devuelve-nil-cuando-el-listado-no-es-error
  (testing "revisar-fnc devuelve nil al recibir un listado que no es un mensaje de error"
    (is (= nil (revisar-fnc '(too-few-args))))
  )
)

(deftest revisar-fnc-devuelve-nil-cuando-el-listado-no-es-lista
  (testing "revisar-fnc devuelve nil al no recibir una lista como parámetro"
    (is (= nil (revisar-fnc '*error*)))
  )
)

(deftest revisar-fnc-devuelve-nil-al-recibir-nil
  (testing "revisar-fnc devuelve nil al recibir nil"
    (is (= nil (revisar-fnc nil)))
  )
)

(deftest revisar-fnc-devuelve-nil-al-recibir-lista-vacia
  (testing "revisar-fnc devuelve nil al recibir una lista vacía"
    (is (= nil (revisar-fnc ())))
  )
)

; tests de revisar-lae

(deftest revisar-lae-devuelve-nil-al-recibir-lista-sin-errores
  (testing "revisar-lae devuelve nil si el listado no contiene ningún error"
    (is (= nil (revisar-lae '(1 2 3))))
  )
)

(deftest revisar-lae-devuelve-nil-al-recibir-nil
  (testing "revisar-lae devuelve nil cuando recibe nil"
    (is (= nil (revisar-lae nil)))
  )
)

(deftest revisar-lae-devuelve-nil-al-recibir-lista-vacia
  (testing "revisar-lae devuelve nil cuando recibe una lista vacía"
    (is (= nil (revisar-lae ())))
  )
)

(deftest revisar-lae-devuelve-error-cuando-recibe-listado-con-un-error
  (testing "revisar-lae devuelve el único error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
  )
)

(deftest revisar-lae-devuelve-primer-error-cuando-recibe-listado-con-multiples-errores
  (testing "revisar-lae devuelve el primer error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))
  )
)

; tests de actualizar-amb

(deftest actualizar-amb-carga-nuevo-par-clave-valor
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo par clave-valor"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
  )
)

(deftest actualizar-amb-reemplaza-clave-preexistente
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo valor de la clave preexistente"
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
  )
)

(deftest actualizar-amb-no-modifica-ambiente-cuando-recibe-error-como-valor
  (testing "actualizar-amb devuelve el ambiente sin modificaciones cuando el nuevo valor para una clave preexistente es un error"
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))))
  )
)

(deftest actualizar-amb-crea-nuevo-ambiente-cuando-recibe-listado-vacio
  (testing "actualizar-amb devuelve un ambiente que sólo posee los pares clave-valor que se le hayan indicado"
    (is (= '(b 7) (actualizar-amb () 'b 7)))
  )
)

; tests de buscar

(deftest buscar-clave-existente-devuelve-valor
  (testing "buscar una clave existente devuelve un valor asociado"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
  )
)

(deftest buscar-clave-inexistente-devuelve-error
  (testing "buscar una clave inexistente devuelve error unbound-symbol"
    (is (= (list '*error* 'unbound-symbol 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)

; tests de fnc-append

(deftest fnc-append-recibir-una-sola-lista-falla
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-few-args) (fnc-append '( (1 2) ))))
  )
)

(deftest fnc-append-recibir-mas-de-dos-listas-falla
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-many-args) (fnc-append '( (1 2) (3) (4 5) (6 7) ))))
  )
)

(deftest fnc-append-recibir-un-elemento-digito-falla
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 3) (fnc-append '( (1 2) 3 ))))
  )
)

(deftest fnc-append-recibir-un-elemento-literal-falla
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 'A) (fnc-append '( (1 2) 'A ))))
  )
)

(deftest fnc-append-recibir-dos-listas-devuelve-concatenacion
  (testing "fnc-append devuelve listas concatenadas cuando recibe dos listas"
    (is (= (list 1 2 3) (fnc-append '( (1 2) (3)))))
  )
)

(deftest fnc-append-recibir-nil-y-lista-no-vacia-devuelve-lista-no-vacia-sin-modificar
  (testing "fnc-append devuelve lista original tras appendear nil"
    (is (= (list 1 2) (fnc-append '( (1 2) nil ))))
  )
)

(deftest fnc-append-recibir-lista-vacia-y-lista-no-vacia-devuelve-lista-no-vacia-sin-modificar
  (testing "fnc-append devuelve lista originalmente no vacía tras appendear lista vacía"
    (is (= (list 1 2) (fnc-append '( () (1 2) ))))
  )
)

(deftest fnc-append-recibir-dos-nil-devuelve-nil
  (testing "fnc-append devuelve nil tras appendear dos nil"
    (is (= nil (fnc-append '( nil nil ))))
  )
)

(deftest fnc-append-recibir-dos-listas-vacias-devuelve-nil
  (testing "fnc-append devuelve nil tras appendear dos listas vacías"
    (is (= nil (fnc-append '( () () ) )))
  )
)

; tests de fnc-env

(deftest fnc-env-recibir-lista-vacia-como-primer-elemento-fusiona-ambientes
    merges-envs-into-one-when-first-elem-is-empty-list
  (testing "fnc-env devuelve un ambiente tras fusionar dos ambientes recibidos"
    (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4))))
  )
)

(deftest fnc-env-recibir-lista-no-vacia-como-primer-elemento-devuelve-error
  (testing "fnc-env devuelve error tras recibir una lista no vacía como primer elemento"
    (is (= (list '*error* 'too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))
  )
)
