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

(deftest igual?-listas-distintas
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
    (is (= true (error? (list '*error* 'too-many-args))))
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

(deftest revisar-fnc-devuelve-lista-de-error
  (testing "revisar-fnc devuelve el listado con mensaje de error recibido"
    (is (= (list '*error* 'too-few-args) (revisar-fnc '(*error* too-few-args))))
  )
)

(deftest revisar-fnc-devuelve-nil-cuando-lista-no-es-error
  (testing "revisar-fnc devuelve nil al recibir un listado que no es un mensaje de error"
    (is (= nil (revisar-fnc '(too-few-args))))
  )
)

(deftest revisar-fnc-devuelve-nil-cuando-lista-no-es-lista
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

(deftest revisar-lae-devuelve-error-cuando-recibe-lista-con-un-error
  (testing "revisar-lae devuelve el único error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
  )
)

(deftest revisar-lae-devuelve-primer-error-cuando-recibe-lista-con-multiples-errores
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

(deftest actualizar-amb-crea-nuevo-ambiente-cuando-recibe-lista-vacia
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

(deftest buscar-clave-existente-con-distinto-case-devuelve-valor
  (testing "buscar una clave existente devuelve un valor asociado"
    (is (= 3 (buscar 'C '(a 1 b 2 c 3 d 4 e 5))))
  )
)

(deftest buscar-clave-inexistente-devuelve-error
  (testing "buscar una clave inexistente devuelve error unbound-symbol"
    (is (= (list '*error* 'unbound-symbol 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)

; tests de fnc-append

(deftest fnc-append-recibir-una-sola-lista-devuelve-error
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-few-args) (fnc-append '( (1 2) ))))
  )
)

(deftest fnc-append-recibir-mas-de-dos-listas-devuelve-error
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-many-args) (fnc-append '( (1 2) (3) (4 5) (6 7) ))))
  )
)

(deftest fnc-append-recibir-un-elemento-digito-devuelve-error
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 3) (fnc-append '( (1 2) 3 ))))
  )
)

(deftest fnc-append-recibir-un-elemento-literal-devuelve-error
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 'A) (fnc-append '( (1 2) A ))))
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
  (testing "fnc-env devuelve un ambiente tras fusionar dos ambientes recibidos"
    (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4))))
  )
)

(deftest fnc-env-recibir-lista-no-vacia-como-primer-elemento-devuelve-error
  (testing "fnc-env devuelve error tras recibir una lista no vacía como primer elemento"
    (is (= (list '*error* 'too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))
  )
)

; tests de fnc-equal

(deftest fnc-equal-devuelve-t-con-digitos-iguales
  (testing "fnc-env devuelve t al recibir un listado de dos dígitos iguales"
    (is (= 't (fnc-equal '(1 1))))
  )
)

(deftest fnc-equal-devuelve-t-con-caracteres-iguales-en-distinto-case
  (testing "fnc-env devuelve t al recibir un listado con dos caracteres iguales en distinto case"
    (is (= 't (fnc-equal '(A a))))
  )
)

(deftest fnc-equal-devuelve-t-con-strings-iguales-en-distinto-case
  (testing "fnc-env devuelve t al recibir un listado con dos strings iguales"
    (is (= 't (fnc-equal '("1" "1"))))
  )
)

(deftest fnc-equal-devuelve-t-comparando-nil-contra-nil-literal
  (testing "fnc-env devuelve t al recibir un listado con un elemento nil y otro nil literal"
    (is (= 't (fnc-equal '(nil NIL))))
  )
)

(deftest fnc-equal-devuelve-nil-con-digitos-distintos
  (testing "fnc-env devuelve nil al recibir un listado de dos dígitos distintos"
    (is (= nil (fnc-equal '(1 2))))
  )
)

(deftest fnc-equal-devuelve-nil-con-caracteres-distintos
  (testing "fnc-env devuelve nil al recibir un listado con dos caracteres distintos"
    (is (= nil (fnc-equal '(A B))))
  )
)

(deftest fnc-equal-devuelve-nil-comparando-digito-contra-string
  (testing "fnc-env devuelve nil al recibir un listado con un string de un digito y el mismo digito"
    (is (= nil (fnc-equal '(A B))))
  )
)

(deftest fnc-equal-devuelve-error-tras-recibir-lista-vacia
  (testing "fnc-env devuelve error al recibir como único parámetro una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-equal ())))
  )
)

(deftest fnc-equal-devuelve-error-tras-recibir-lista-de-longitud-uno
  (testing "fnc-env devuelve error al recibir un listado de longitud uno"
    (is (= (list '*error* 'too-few-args) (fnc-equal '(A))))
  )
)

(deftest fnc-equal-devuelve-error-tras-recibir-lista-de-longitud-mayor-a-2
  (testing "fnc-env devuelve error al recibir un listado de longitud mayor a 2"
    (is (= (list '*error* 'too-many-args) (fnc-equal '(A a A))))
  )
)

; tests de fnc-read

(deftest fnc-read-recibe-digito-y-lo-retorna
  (testing "fnc-read recibe un dígito y lo devuelve correctamente"
    (is (= 1 (with-in-str "1" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-caracter-y-lo-retorna
  (testing "fnc-read recibe un caracter y lo devuelve correctamente"
    (is (= 'a (with-in-str "a" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-cadena-y-la-retorna
  (testing "fnc-read recibe una cadena y la devuelve correctamente"
    (is (= '"hola" (with-in-str "\"hola\"" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-cadena-lista-y-la-retorna
  (testing "fnc-read recibe una lista y la devuelve correctamente"
    (is (= '(hola mundo) (with-in-str "(hola mundo)" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-cadena-lista-con-newline-y-la-retorna
  (testing "fnc-read recibe una lista y la devuelve correctamente"
    (is (= '(hola mundo) (with-in-str "(hola\nmundo)" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-lista-vacia-y-retorna-nil
  (testing "fnc-read recibe una lista vacía y devuelve nil"
    (is (= nil (with-in-str "()" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-nil-y-retorna-nil
  (testing "fnc-read recibe nil y devuelve nil"
    (is (= nil (with-in-str "nil" (fnc-read '()))))
  )
)

(deftest fnc-read-recibe-lista-no-vacia-como-parametro-y-devuelve-error
  (testing "fnc-read recibe lista con un elemento como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (with-in-str "test-input" (fnc-read '(1)))))
  )
)

(deftest fnc-read-recibe-lista-con-dos-elementos-como-parametro-y-devuelve-error
  (testing "fnc-read recibe una lista con dos elementos como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (with-in-str "test-input" (fnc-read '(1 2)))))
  )
)

; tests de fnc-terpri

(deftest fnc-terpri-recibe-lista-vacia-e-imprime-newline
  (testing "fnc-terpri recibe una lista vacía e imprime salto de línea"
    (is (= "\n" (with-out-str (fnc-terpri '()))))
  )
)

(deftest fnc-terpri-recibe-lista-vacia-y-devuelve-nil
  (testing "fnc-terpri recibe una lista vacía devuelve nil"
    (is (= nil (fnc-terpri '())))
  )
)

(deftest fnc-terpri-recibe-lista-no-vacia-como-parametro-y-devuelve-error
  (testing "fnc-terpri recibe lista con un elemento como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (fnc-terpri '(1))))
  )
)

(deftest fnc-terpri-recibe-lista-con-dos-elementos-como-parametro-y-devuelve-error
  (testing "fnc-terpri recibe una lista con dos elementos como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (fnc-terpri '(1 2))))
  )
)

; tests de fnc-add

(deftest fnc-add-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-add devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-add ())))
  )
)

(deftest fnc-add-recibe-lista-de-longitud-1-y-devuelve-error
  (testing "fnc-add devuelve error cuando recibe una lista con un elemento"
    (is (= (list '*error* 'too-few-args) (fnc-add '(3))))
  )
)

(deftest fnc-add-recibe-lista-de-longitud-2-y-retorna-suma
  (testing "fnc-add devuelve suma cuando recibe una lista con dos elementos"
    (is (= 7 (fnc-add '(3 4))))
  )
)

(deftest fnc-add-recibe-lista-de-longitud-3-y-retorna-suma
  (testing "fnc-add devuelve suma cuando recibe una lista con tres elementos"
    (is (= 12 (fnc-add '(3 4 5))))
  )
)

(deftest fnc-add-recibe-lista-de-longitud-4-y-retorna-suma
  (testing "fnc-add devuelve suma cuando recibe una lista con cuatro elementos"
    (is (= 18 (fnc-add '(3 4 5 6))))
  )
)

(deftest fnc-add-recibe-lista-con-primer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-add devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(A 4 5 6))))
  )
)

(deftest fnc-add-recibe-lista-con-segundo-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-add devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 A 5 6))))
  )
)

(deftest fnc-add-recibe-lista-con-tercer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-add devuelve error cuando recibe una lista cuyo tercer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 4 A 6))))
  )
)

; tests de fnc-sub

(deftest fnc-sub-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-sub devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-sub ())))
  )
)

(deftest fnc-sub-recibe-lista-de-longitud-1-y-devuelve-signo-opuesto
  (testing "fnc-sub devuelve elemento con signo opuesto cuando recibe una lista con un elemento"
    (is (= -3 (fnc-sub '(3))))
  )
)

(deftest fnc-sub-recibe-lista-de-longitud-2-y-retorna-resta
  (testing "fnc-sub devuelve resta cuando recibe una lista con dos elementos"
    (is (= -1 (fnc-sub '(3 4))))
  )
)

(deftest fnc-sub-recibe-lista-de-longitud-3-y-retorna-resta
  (testing "fnc-sub devuelve resta cuando recibe una lista con tres elementos"
    (is (= -6 (fnc-sub '(3 4 5))))
  )
)

(deftest fnc-sub-recibe-lista-de-longitud-4-y-retorna-resta
  (testing "fnc-sub devuelve resta cuando recibe una lista con cuatro elementos"
    (is (= -12 (fnc-sub '(3 4 5 6))))
  )
)

(deftest fnc-sub-recibe-lista-con-primer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(A 4 5 6))))
  )
)

(deftest fnc-sub-recibe-lista-con-segundo-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 A 5 6))))
  )
)

(deftest fnc-sub-recibe-lista-con-tercer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo tercer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 4 A 6))))
  )
)

; tests de fnc-lt

(deftest fnc-lt-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-lt devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-lt ())))
  )
)

(deftest fnc-lt-recibe-lista-de-longitud-mayor-a-2-y-devuelve-error
  (testing "fnc-lt devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-lt '(1 2 3))))
  )
)

(deftest fnc-lt-recibe-lista-de-longitud-1-y-devuelve-error
  (testing "fnc-lt devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-lt '(1))))
  )
)

(deftest fnc-lt-recibe-lista-con-primer-elemento-menor-y-devuelve-t
  (testing "fnc-lt devuelve t cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= 't (fnc-lt '(1 2))))
  )
)

(deftest fnc-lt-recibe-lista-con-2-elementos-iguales-y-devuelve-nil
  (testing "fnc-lt devuelve nil cuando recibe una lista con dos elementos iguales"
    (is (= nil (fnc-lt '(1 1))))
  )
)

(deftest fnc-lt-recibe-lista-con-segundo-elemento-menor-y-devuelve-nil
  (testing "fnc-lt devuelve nil cuando recibe una lista cuyo segundo elemento es menor que el primero"
    (is (= nil (fnc-lt '(2 1))))
  )
)

(deftest fnc-lt-recibe-lista-con-primer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-lt devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(A 1))))
  )
)

(deftest fnc-lt-recibe-lista-con-segundo-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-lt devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(1 A))))
  )
)

; tests de fnc-gt

(deftest fnc-gt-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-gt devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-gt ())))
  )
)

(deftest fnc-gt-recibe-lista-de-longitud-mayor-a-2-y-devuelve-error
  (testing "fnc-gt devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-gt '(1 2 3))))
  )
)

(deftest fnc-gt-recibe-lista-de-longitud-1-y-devuelve-error
  (testing "fnc-gt devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-gt '(1))))
  )
)

(deftest fnc-gt-recibe-lista-con-primer-elemento-menor-y-devuelve-nil
  (testing "fnc-gt devuelve nil cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= nil (fnc-gt '(1 2))))
  )
)

(deftest fnc-gt-recibe-lista-con-2-elementos-iguales-y-devuelve-nil
  (testing "fnc-gt devuelve nil cuando recibe una lista con dos elementos iguales"
    (is (= nil (fnc-gt '(1 1))))
  )
)

(deftest fnc-gt-recibe-lista-con-segundo-elemento-menor-y-devuelve-t
  (testing "fnc-gt devuelve t cuando recibe una lista cuyo segundo elemento es menor que el primero"
    (is (= 't (fnc-gt '(2 1))))
  )
)

(deftest fnc-gt-recibe-lista-con-primer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-gt devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-gt '(A 1))))
  )
)

(deftest fnc-gt-recibe-lista-con-segundo-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-gt devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-gt '(1 A))))
  )
)

; tests de fnc-ge

(deftest fnc-ge-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-ge devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-ge ())))
  )
)

(deftest fnc-ge-recibe-lista-de-longitud-mayor-a-2-y-devuelve-error
  (testing "fnc-ge devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-ge '(1 2 3))))
  )
)

(deftest fnc-ge-recibe-lista-de-longitud-1-y-devuelve-error
  (testing "fnc-ge devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-ge '(1))))
  )
)

(deftest fnc-ge-recibe-lista-con-primer-elemento-mayor-o-igual-y-devuelve-t
  (testing "fnc-ge devuelve t cuando recibe una lista cuyo primer elemento es mayor o igual que el segundo"
    (is (= 't (fnc-ge '(2 1))))
  )
)

(deftest fnc-ge-recibe-lista-con-2-elementos-iguales-y-devuelve-t
  (testing "fnc-ge devuelve t cuando recibe una lista con dos elementos iguales"
    (is (= 't (fnc-ge '(1 1))))
  )
)

(deftest fnc-ge-recibe-lista-con-primer-elemento-menor-y-devuelve-nil
  (testing "fnc-ge devuelve nil cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= nil (fnc-ge '(1 2))))
  )
)

(deftest fnc-ge-recibe-lista-con-primer-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-ge devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-ge '(A 1))))
  )
)

(deftest fnc-ge-recibe-lista-con-segundo-elemento-caracter-no-numerico-y-devuelve-error
  (testing "fnc-ge devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-ge '(1 A))))
  )
)

; tests de fnc-reverse

(deftest fnc-reverse-recibe-lista-vacia-y-devuelve-error
  (testing "fnc-reverse devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-reverse ())))
  )
)

(deftest fnc-reverse-recibe-lista-con-mas-de-1-elemento-y-devuelve-error
  (testing "fnc-reverse devuelve error cuando recibe una lista con más de un elemento"
    (is (= (list '*error* 'too-many-args) (fnc-reverse '((1 2 3)(4)))))
  )
)

(deftest fnc-reverse-recibe-lista-con-unico-elemento-digito-devuelve-error
  (testing "fnc-reverse devuelve error cuando recibe una lista con un único elemento dígito"
    (is (= (list '*error* 'list 'expected 1) (fnc-reverse '(1))))
  )
)

(deftest fnc-reverse-recibe-lista-con-unico-elemento-caracter-devuelve-error
  (testing "fnc-reverse devuelve error cuando recibe una lista con un único elemento caracter"
    (is (= (list '*error* 'list 'expected 'A) (fnc-reverse '(A))))
  )
)

(deftest fnc-reverse-recibe-lista-con-unica-lista-con-digito-devuelve-lista
  (testing "fnc-reverse devuelve lista original cuando recibe una lista con un único elemento lista con un dígito"
    (is (= (list 1) (fnc-reverse '((1)))))
  )
)

(deftest fnc-reverse-recibe-lista-con-unica-lista-digito-devuelve-lista-invertida
  (testing "fnc-reverse devuelve lista invertida cuando recibe una lista con un único elemento lista con  dígitos"
    (is (= (list 3 2 1) (fnc-reverse '((1 2 3)))))
  )
)

; tests de evaluar-escalar

(deftest evaluar-escalar-recibe-digito-y-lo-devuelve
  (testing "evaluar-escalar"
    (is (= '(32 (v 1 w 3 x 6)) (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-string-y-lo-devuelve
  (testing "evaluar-escalar"
    (is (= '("chau" (v 1 w 3 x 6)) (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-simbolo-y-devuelve-valor-asociado-en-ambiente-global
  (testing "evaluar-escalar recibe un literal y devuelve valor asociado encontrado en ambiente global"
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-simbolo-en-mayuscula-y-devuelve-valor-asociado-en-ambiente-global
  (testing "evaluar-escalar recibe un literal en mayúscula y devuelve valor asociado encontrado sólo en ambiente global ignorando case"
     (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-simbolo-y-devuelve-valor-asociado-en-ambiente-local
  (testing "evaluar-escalar recibe símbolo y devuelve el valor asociado encontrado sólo en el ambiente local"
    (is (= '(3 (v 1 w 3 x 6)) (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-simbolo-presente-en-ambos-ambientes-y-devuelve-valor-asociado-en-ambiente-global
  (testing "evaluar-escalar recibe símbolo presente en ambos ambientes y devuelve valor asociado del ambiente global"
    (is (= '(5 (v 1 w 3 x 6)) (evaluar-escalar 'X '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-escalar-recibe-simbolo-inexistente-en-ambos-ambientes-y-devuelve-error
  (testing "evaluar-escalar recibe símbolo que no se encuentra en ningún ambiente y devuelve error"
    (is (= '((*error* unbound-symbol n) (v 1 w 3 x 6)) (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

; tests de evaluar-de

(deftest evaluar-de-1
  (testing "evaluar-de función con tres elementos"
    (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(de f (x)) '(x 1))))
  )
)

(deftest evaluar-de-2
  (testing "evaluar-de función con cuatro elementos"
    (is (= '(f (x 1 f (lambda (x) 2))) (evaluar-de '(de f (x) 2) '(x 1))))
  )
)

(deftest evaluar-de-3
  (testing "evaluar-de cuarto elemento de función es lista"
    (is (= '(f (x 1 f (lambda (x) (+ x 1)))) (evaluar-de '(de f (x) (+ x 1)) '(x 1))))
  )
)

(deftest evaluar-de-4
  (testing "evaluar-de función con cuatro elementos (hay dos listas)"
    (is (= '(f (x 1 f (lambda (x y) (+ x y)))) (evaluar-de '(de f (x y) (+ x y)) '(x 1))))
  )
)

(deftest evaluar-de-5
  (testing "evaluar-de función con 6 elementos (hay tres listas y un literal)"
    (is (= '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y))) (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1))))
  )
)

(deftest evaluar-de-error-1
  (testing "evaluar-de longitud función tiene un único elemento"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de) '(x 1))))
  )
)

(deftest evaluar-de-error-2
  (testing "evaluar-de función tiene dos elementos (el segundo es un símbolo)"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de f) '(x 1))))
  )
)

(deftest evaluar-de-error-3
  (testing "evaluar-de tercer elemento de función no es un símbolo"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2) '(x 1))))
  )
)

(deftest evaluar-de-error-4
  (testing "evaluar-de tercer y cuarto elemento de función no son símbolos"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2 3) '(x 1))))
  )
)

(deftest evaluar-de-error-5
  (testing "evaluar-de tiene dos elementos (el segundo es una lista)"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de (f)) '(x 1))))
  )
)

(deftest evaluar-de-error-6
  (testing "evaluar-de tercer elemento es un símbolo"
    (is (= '((*error* list expected x) (x 1)) (evaluar-de '(de 2 x) '(x 1))))
  )
)

(deftest evaluar-de-error-7
  (testing "evaluar-de segundo elemento no es un símbolo"
    (is (= '((*error* symbol expected 2) (x 1)) (evaluar-de '(de 2 (x)) '(x 1))))
  )
)

(deftest evaluar-de-error-8
  (testing "evaluar-de el segundo elemento es nil"
    (is (= '((*error* cannot-set nil) (x 1)) (evaluar-de '(de nil (x) 2) '(x 1))))
  )
)

; tests de evaluar-if

; importante: en los mensajes de testing sólo se aclara lo que se retorna como primer elemento
; del resultado ya que el segundo elemento es siempre el ambiente actualizado.

(deftest evaluar-if-1
  (testing "evaluar-if con único elemento t retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-2
  (testing "evaluar-if con único elemento dígito retorna nil"
 (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-3
  (testing "evaluar-if con único elemento nil"
(is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-4
  (testing "evaluar-if con único elemento clave existente retorna nil"
(is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-5
  (testing "evaluar-if con dos elementos (clave t existente en ambiente global y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-6
  (testing "evaluar-if con dos elementos (clave z existente en ambiente local y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-7
  (testing "evaluar-if con dos elementos (clave w existente en ambiente global y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-8
  (testing "evaluar-if con dos elementos (clave inexistente) retorna error"
    (is (= '((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-9
  (testing "evaluar-if con dos elementos y primer elemento nil retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-10
  (testing "evaluar-if con tres elementos y primer elemento nil retorna valor asociado a última clave existente en ambiente local"
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-11
  (testing "evaluar-if con seis elementos y primer elemento nil retorna valor asociado a última clave existente en ambiente local"
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-12
  (testing "evaluar-if con tres elementos y primer elemento nil retorna valor asociado a ultima clave existente en ambiente global"
    (is (= '(3 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-13
  (testing "evaluar-if con tres elementos y primer elemento nil retorna último elemento"
    (is (= '(8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-14
  (testing "evaluar-if con tres elementos y primer elemento nil retorna último elemento"
    (is (= '((8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  )
)

(deftest evaluar-if-15
  (testing "evaluar-if con tres elementos y primer elemento condición true retorna error por clave desconocida a"
    (is (= '((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"
    ))))
  )
)

(deftest evaluar-if-16
  (testing "evaluar-if con tres elementos y primer elemento condición nil retorna último elemento"
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

(deftest evaluar-if-17
  (testing "evaluar-if con tres elementos y primer elemento condición nil retorna primer elemento del resultado de evaluar la expresión del último resultado"
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8)) (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)
