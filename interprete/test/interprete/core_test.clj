(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

; tests de controlar-aridad

(deftest controlar-aridad-tests
  (testing "controlar-aridad devuelve longitud esperada"
    (is (= 3 (controlar-aridad '(a b c) 3)))
  )
  (testing "controlar-aridad devuelve error al recibir demasiados argumentos"
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
  )
  (testing "controlar-aridad devuelve error al recibir insuficientes argumentos"
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  )
)

; tests de igual?

(deftest igual?-tests
  (testing "igual? devuelve true comparando dígitos iguales"
    (is (= true (igual? 1 1)))
  )
  (testing "igual? devuelve false comparando dígitos distintos"
    (is (= false (igual? 1 2)))
  )
  (testing "igual? devuelve true comparando literales de iguales caracteres con distinto case"
    (is (= true (igual? 'a 'A)))
  )
  (testing "igual? devuelve false comparando caracteres distintos"
    (is (= false (igual? 'a 'b)))
  )
  (testing "igual? devuelve true comparando listas de caracteres iguales"
    (is (= true (igual? '(a b c) '(a b c))))
  )
  (testing "igual? devuelve true comparando listas de caracteres iguales pero con distinto case"
    (is (= true (igual? '(a b c) '(A B C))))
  )
  (testing "igual? devuelve false comparando listas de caracteres distintas"
    (is (= false (igual? '(a b c) '(a b d))))
  )
  (testing "igual? devuelve true comparando nils"
    (is (= true (igual? nil nil)))
  )
  (testing "igual? devuelve true comparando nil contra el literal 'NIL"
    (is (= true (igual? nil 'NIL)))
  )
  (testing "igual? devuelve true comparando nil contra una lista vacía"
    (is (= true (igual? nil ())))
  )
  (testing "igual? devuelve true comparando listas vacías"
    (is (= true (igual? () ())))
  )
  (testing "igual? devuelve false comparando una lista vacía contra una lista cuyo único elemento es nil"
    (is (= false (igual? () '(nil))))
  )
  (testing "igual? devuelve true comparando strings iguales"
    (is (= true (igual? "a" "a")))
  )
  (testing "igual? devuelve false comparando strings con distinto case"
    (is (= false (igual? "a" "A")))
  )
  (testing "igual? devuelve false comparando strings distintos"
    (is (= false (igual? "a" "b")))
  )
)

; tests de error?

(deftest error?-tests
  (testing "error? devuelve true al recibir una lista con primer elemento \"*error*\""
    (is (= true (error? '(*error* too-few-args))))
  )
  (testing "error? devuelve true al recibir una lista con varios elementos donde el primero es \"*error*\""
    (is (= true (error? (list '*error* 'too-many-args))))
  )
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"ERROR\""
    (is (= true (error? '(*ERROR* too-few-args))))
  )
  (testing "error? devuelve true al recibir una lista cuya primera palabra es \"Error\""
    (is (= true (error? '(*Error* too-few-args))))
  )
  (testing "error? devuelve true al recibir una lista con un único elemento \"*error*\""
    (is (= true (error? '(*error*))))
  )
  (testing "error? devuelve false al recibir una lista con un único elemento distinto de \"*error*\" (o alguna de sus variaciones en distinto case)"
    (is (= false (error? (list 'too-few-args))))
  )
  (testing "error? devuelve false al no recibir el error dentro de una lista"
    (is (= false (error? '*error*)))
  )
  (testing "error? devuelve false al recibir una lista sin mensaje de error"
    (is (= false (error? ())))
  )
  (testing "error? devuelve false al recibir nil"
    (is (= false (error? ())))
  )
)

; tests de revisar-fnc

(deftest revisar-fnc-tests
  (testing "revisar-fnc devuelve el listado con mensaje de error recibido"
    (is (= (list '*error* 'too-few-args) (revisar-fnc '(*error* too-few-args))))
  )
  (testing "revisar-fnc devuelve nil al recibir un listado que no es un mensaje de error"
    (is (= nil (revisar-fnc '(too-few-args))))
  )
  (testing "revisar-fnc devuelve nil al no recibir una lista como parámetro"
    (is (= nil (revisar-fnc '*error*)))
  )
  (testing "revisar-fnc devuelve nil al recibir nil"
    (is (= nil (revisar-fnc nil)))
  )
  (testing "revisar-fnc devuelve nil al recibir una lista vacía"
    (is (= nil (revisar-fnc ())))
  )
)

; tests de revisar-lae

(deftest revisar-lae-tests
  (testing "revisar-lae devuelve nil si el listado no contiene ningún error"
    (is (= nil (revisar-lae '(1 2 3))))
  )
  (testing "revisar-lae devuelve nil cuando recibe nil"
    (is (= nil (revisar-lae nil)))
  )
  (testing "revisar-lae devuelve nil cuando recibe una lista vacía"
    (is (= nil (revisar-lae ())))
  )
  (testing "revisar-lae devuelve el único error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) 3))))
  )
  (testing "revisar-lae devuelve el primer error que hay dentro de la lista"
    (is (= (list '*error* 'too-few-args) (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))
  )
)

; tests de actualizar-amb

(deftest actualizar-amb-tests
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo par clave-valor"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
  )
  (testing "actualizar-amb devuelve el ambiente actualizado con el nuevo valor de la clave preexistente"
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
  )
  (testing "actualizar-amb devuelve el ambiente sin modificaciones cuando el nuevo valor para una clave preexistente es un error"
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))))
  )
  (testing "actualizar-amb devuelve un ambiente que sólo posee los pares clave-valor que se le hayan indicado"
    (is (= '(b 7) (actualizar-amb () 'b 7)))
  )
  (testing "actualizar-amb usando clave inexistente n mayúscula devuelve el ambiente actualizado con el nuevo par clave-valor en minúscula"
    (is (= '(a 1 b 2 c 3 e 0) (actualizar-amb '(a 1 b 2 c 3) 'E 0)))
  )
  (testing "actualizar-amb con clave en mayúscula devuelve el ambiente actualizado con el nuevo valor de la clave preexistente en minúscula"
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'B 4)))
  )
  (testing "actualizar-amb devuelve un ambiente que sólo posee los pares clave-valor que se le hayan indicado pero en minúscula"
    (is (= '(b 7) (actualizar-amb () 'B 7)))
  )
)

; tests de buscar

(deftest buscar-clave-tests
  (testing "buscar una clave existente devuelve un valor asociado"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
  )
  (testing "buscar una clave existente en mayúscula devuelve un valor asociado a la misma clave en minúscula"
    (is (= 3 (buscar 'C '(a 1 b 2 c 3 d 4 e 5))))
  )
  (testing "buscar una clave existente devuelve un valor asociado lista"
    (is (= '(1 2 3) (buscar 'a '(a (1 2 3) b 2 c 3 d 4 e 5))))
  )
  (testing "buscar una clave inexistente devuelve error unbound-symbol"
    (is (= (list '*error* 'unbound-symbol 'f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
  )
)

; tests de fnc-append

(deftest fnc-append-tests
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-few-args) (fnc-append '( (1 2) ))))
  )
  (testing "fnc-append devuelve error cuando recibe una sola lista"
    (is (= (list '*error* 'too-many-args) (fnc-append '( (1 2) (3) (4 5) (6 7) ))))
  )
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 3) (fnc-append '( (1 2) 3 ))))
  )
  (testing "fnc-append devuelve error cuando recibe un valor que no es lista"
    (is (= (list '*error* 'list 'expected 'A) (fnc-append '( (1 2) A ))))
  )
  (testing "fnc-append devuelve listas concatenadas cuando recibe dos listas"
    (is (= (list 1 2 3) (fnc-append '( (1 2) (3)))))
  )
  (testing "fnc-append devuelve lista original tras appendear nil"
    (is (= (list 1 2) (fnc-append '( (1 2) nil ))))
  )
  (testing "fnc-append devuelve lista originalmente no vacía tras appendear lista vacía"
    (is (= (list 1 2) (fnc-append '( () (1 2) ))))
  )
  (testing "fnc-append devuelve nil tras appendear dos nil"
    (is (= nil (fnc-append '( nil nil ))))
  )
  (testing "fnc-append devuelve nil tras appendear dos listas vacías"
    (is (= nil (fnc-append '( () () ) )))
  )
  (testing "fnc-append entre listas con caracteres alfabéticos"
    (is (= (list 'a 'b 'c 'd) (fnc-append '( (a b) (c d) ))))
  )
  (testing "fnc-append no modifica case de las listas recibidas"
    (is (= (list 'A 'b 'C 'd) (fnc-append '( (A b) (C d) ))))
  )
)

; tests de fnc-env

(deftest fnc-env-tests
  (testing "fnc-env devuelve un ambiente tras fusionar dos ambientes recibidos"
    (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4))))
  )
  (testing "fnc-env devuelve error tras recibir una lista no vacía como primer elemento"
    (is (= (list '*error* 'too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))
  )
  (testing "fnc-env no altera case tras fusionar dos ambientes recibidos"
    (is (= '(A 1 b 2 C 3 d 4) (fnc-env () '(A 1 b 2) '(C 3 d 4))))
  )
)

; tests de fnc-equal

(deftest fnc-equal-tests
  (testing "fnc-env devuelve t al recibir un listado de dos dígitos iguales"
    (is (= 't (fnc-equal '(1 1))))
  )
  (testing "fnc-env devuelve t al recibir un listado con dos caracteres iguales en distinto case"
    (is (= 't (fnc-equal '(A a))))
  )
  (testing "fnc-env devuelve t al recibir un listado con dos strings iguales"
    (is (= 't (fnc-equal '("1" "1"))))
  )
  (testing "fnc-env devuelve t al recibir un listado con un elemento nil y otro nil literal"
    (is (= 't (fnc-equal '(nil NIL))))
  )
  (testing "fnc-env devuelve nil al recibir un listado de dos dígitos distintos"
    (is (= nil (fnc-equal '(1 2))))
  )
  (testing "fnc-env devuelve nil al recibir un listado con dos caracteres distintos"
    (is (= nil (fnc-equal '(A B))))
  )
  (testing "fnc-env devuelve nil al recibir un listado con un string de un digito y el mismo digito"
    (is (= nil (fnc-equal '(A B))))
  )
  (testing "fnc-env devuelve error al recibir como único parámetro una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-equal ())))
  )
  (testing "fnc-env devuelve error al recibir un listado de longitud uno"
    (is (= (list '*error* 'too-few-args) (fnc-equal '(A))))
  )
  (testing "fnc-env devuelve error al recibir un listado de longitud mayor a 2"
    (is (= (list '*error* 'too-many-args) (fnc-equal '(A a A))))
  )
)

; tests de fnc-read

(deftest fnc-read-tests
  (testing "fnc-read recibe un dígito y lo devuelve correctamente"
    (is (= 1 (with-in-str "1" (fnc-read '()))))
  )
  (testing "fnc-read recibe un caracter y lo devuelve correctamente"
    (is (= 'a (with-in-str "a" (fnc-read '()))))
  )
  (testing "fnc-read recibe una cadena y la devuelve correctamente"
    (is (= '"hola" (with-in-str "\"hola\"" (fnc-read '()))))
  )
  (testing "fnc-read recibe una lista y la devuelve correctamente"
    (is (= '(hola mundo) (with-in-str "(hola mundo)" (fnc-read '()))))
  )
  (testing "fnc-read recibe una lista y la devuelve correctamente"
    (is (= '(hola mundo) (with-in-str "(hola\nmundo)" (fnc-read '()))))
  )
  (testing "fnc-read recibe una lista vacía y devuelve nil"
    (is (= nil (with-in-str "()" (fnc-read '()))))
  )
  (testing "fnc-read recibe nil y devuelve nil"
    (is (= nil (with-in-str "nil" (fnc-read '()))))
  )
  (testing "fnc-read recibe lista con un elemento como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (with-in-str "test-input" (fnc-read '(1)))))
  )
  (testing "fnc-read recibe una lista con dos elementos como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (with-in-str "test-input" (fnc-read '(1 2)))))
  )
)

; tests de fnc-terpri-tests

(deftest fnc-terpri-tests
  (testing "fnc-terpri recibe una lista vacía e imprime salto de línea"
    (is (= "\n" (with-out-str (fnc-terpri '()))))
  )
  (testing "fnc-terpri recibe una lista vacía devuelve nil"
    (is (= nil (fnc-terpri '())))
  )
  (testing "fnc-terpri recibe lista con un elemento como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (fnc-terpri '(1))))
  )
  (testing "fnc-terpri recibe una lista con dos elementos como parámetro y falla"
    (is (= (list '*error* 'not-implemented) (fnc-terpri '(1 2))))
  )
)

; tests de fnc-add

(deftest fnc-add-tests
  (testing "fnc-add devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-add ())))
  )
  (testing "fnc-add devuelve error cuando recibe una lista con un elemento"
    (is (= (list '*error* 'too-few-args) (fnc-add '(3))))
  )
  (testing "fnc-add devuelve suma cuando recibe una lista con dos elementos"
    (is (= 7 (fnc-add '(3 4))))
  )
  (testing "fnc-add devuelve suma cuando recibe una lista con tres elementos"
    (is (= 12 (fnc-add '(3 4 5))))
  )
  (testing "fnc-add devuelve suma cuando recibe una lista con cuatro elementos"
    (is (= 18 (fnc-add '(3 4 5 6))))
  )
  (testing "fnc-add devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(A 4 5 6))))
  )
  (testing "fnc-add devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 A 5 6))))
  )
  (testing "fnc-add devuelve error cuando recibe una lista cuyo tercer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-add '(3 4 A 6))))
  )
)

; tests de fnc-sub

(deftest fnc-sub-tests
  (testing "fnc-sub devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-sub ())))
  )
  (testing "fnc-sub devuelve elemento con signo opuesto cuando recibe una lista con un elemento"
    (is (= -3 (fnc-sub '(3))))
  )
  (testing "fnc-sub devuelve resta cuando recibe una lista con dos elementos"
    (is (= -1 (fnc-sub '(3 4))))
  )
  (testing "fnc-sub devuelve resta cuando recibe una lista con tres elementos"
    (is (= -6 (fnc-sub '(3 4 5))))
  )
  (testing "fnc-sub devuelve resta cuando recibe una lista con cuatro elementos"
    (is (= -12 (fnc-sub '(3 4 5 6))))
  )
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(A 4 5 6))))
  )
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 A 5 6))))
  )
  (testing "fnc-sub devuelve error cuando recibe una lista cuyo tercer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-sub '(3 4 A 6))))
  )
)

; tests de fnc-lt

(deftest fnc-lt-tests
  (testing "fnc-lt devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-lt ())))
  )
  (testing "fnc-lt devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-lt '(1 2 3))))
  )
  (testing "fnc-lt devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-lt '(1))))
  )
  (testing "fnc-lt devuelve t cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= 't (fnc-lt '(1 2))))
  )
  (testing "fnc-lt devuelve nil cuando recibe una lista con dos elementos iguales"
    (is (= nil (fnc-lt '(1 1))))
  )
  (testing "fnc-lt devuelve nil cuando recibe una lista cuyo segundo elemento es menor que el primero"
    (is (= nil (fnc-lt '(2 1))))
  )
  (testing "fnc-lt devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(A 1))))
  )
  (testing "fnc-lt devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-lt '(1 A))))
  )
)

; tests de fnc-gt

(deftest fnc-gt-tests
  (testing "fnc-gt devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-gt ())))
  )
  (testing "fnc-gt devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-gt '(1 2 3))))
  )
  (testing "fnc-gt devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-gt '(1))))
  )
  (testing "fnc-gt devuelve nil cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= nil (fnc-gt '(1 2))))
  )
  (testing "fnc-gt devuelve nil cuando recibe una lista con dos elementos iguales"
    (is (= nil (fnc-gt '(1 1))))
  )
  (testing "fnc-gt devuelve t cuando recibe una lista cuyo segundo elemento es menor que el primero"
    (is (= 't (fnc-gt '(2 1))))
  )
  (testing "fnc-gt devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-gt '(A 1))))
  )
  (testing "fnc-gt devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-gt '(1 A))))
  )
)

; tests de fnc-ge

(deftest fnc-ge-tests
  (testing "fnc-ge devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-ge ())))
  )
  (testing "fnc-ge devuelve error cuando recibe una lista de más de dos elementos"
    (is (= (list '*error* 'too-many-args) (fnc-ge '(1 2 3))))
  )
  (testing "fnc-ge devuelve error cuando recibe una lista con un único elemento"
    (is (= (list '*error* 'too-few-args) (fnc-ge '(1))))
  )
  (testing "fnc-ge devuelve t cuando recibe una lista cuyo primer elemento es mayor o igual que el segundo"
    (is (= 't (fnc-ge '(2 1))))
  )
  (testing "fnc-ge devuelve t cuando recibe una lista con dos elementos iguales"
    (is (= 't (fnc-ge '(1 1))))
  )
  (testing "fnc-ge devuelve nil cuando recibe una lista cuyo primer elemento es menor que el segundo"
    (is (= nil (fnc-ge '(1 2))))
  )
  (testing "fnc-ge devuelve error cuando recibe una lista cuyo primer elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-ge '(A 1))))
  )
  (testing "fnc-ge devuelve error cuando recibe una lista cuyo segundo elemento es un caracter no numérico"
    (is (= (list '*error* 'number-expected 'A) (fnc-ge '(1 A))))
  )
)

; tests de fnc-reverse

(deftest fnc-reverse-tests
  (testing "fnc-reverse devuelve error cuando recibe una lista vacía"
    (is (= (list '*error* 'too-few-args) (fnc-reverse ())))
  )
  (testing "fnc-reverse devuelve error cuando recibe una lista con más de un elemento"
    (is (= (list '*error* 'too-many-args) (fnc-reverse '((1 2 3)(4)))))
  )
  (testing "fnc-reverse devuelve error cuando recibe una lista con un único elemento dígito"
    (is (= (list '*error* 'list 'expected 1) (fnc-reverse '(1))))
  )
  (testing "fnc-reverse devuelve error cuando recibe una lista con un único elemento caracter"
    (is (= (list '*error* 'list 'expected 'A) (fnc-reverse '(A))))
  )
  (testing "fnc-reverse devuelve lista original cuando recibe una lista con un único elemento lista con un dígito"
    (is (= (list 1) (fnc-reverse '((1)))))
  )
  (testing "fnc-reverse devuelve lista invertida cuando recibe una lista con un único elemento lista con  dígitos"
    (is (= (list 3 2 1) (fnc-reverse '((1 2 3)))))
  )
)

; tests de evaluar-escalar

(deftest evaluar-escalar-tests
  (testing "evaluar-escalar devuelve escalar evaluado"
    (is (= '(32 (v 1 w 3 x 6)) (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar devuelve string evaluado"
    (is (= '("chau" (v 1 w 3 x 6)) (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe un literal y devuelve valor asociado encontrado en ambiente global"
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe un literal en mayúscula y devuelve valor asociado encontrado en ambiente global"
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe un literal en mayúscula y devuelve valor asociado encontrado sólo en ambiente global ignorando case"
     (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe símbolo y devuelve el valor asociado encontrado sólo en el ambiente local"
    (is (= '(3 (v 1 w 3 x 6)) (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe símbolo presente en ambos ambientes y devuelve valor asociado del ambiente global"
    (is (= '(5 (v 1 w 3 x 6)) (evaluar-escalar 'X '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-escalar recibe símbolo que no se encuentra en ningún ambiente y devuelve error"
    (is (= '((*error* unbound-symbol n) (v 1 w 3 x 6)) (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

; tests de evaluar-de

(deftest evaluar-de-tests
  (testing "evaluar-de función con tres elementos"
    (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(de f (x)) '(x 1))))
  )
  (testing "evaluar-de función con tres elementos en mayúscula"
    (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(DE F (X)) '(X 1))))
  )
  (testing "evaluar-de función con cuatro elementos"
    (is (= '(f (x 1 f (lambda (x) 2))) (evaluar-de '(de f (x) 2) '(x 1))))
  )
  (testing "evaluar-de cuarto elemento de función es lista"
    (is (= '(f (x 1 f (lambda (x) (+ x 1)))) (evaluar-de '(de f (x) (+ x 1)) '(x 1))))
  )
  (testing "evaluar-de función con cuatro elementos (hay dos listas)"
    (is (= '(f (x 1 f (lambda (x y) (+ x y)))) (evaluar-de '(de f (x y) (+ x y)) '(x 1))))
  )
  (testing "evaluar-de función con 6 elementos (hay tres listas y un literal)"
    (is (= '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y))) (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1))))
  )
  (testing "evaluar-de longitud función tiene un único elemento"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de) '(x 1))))
  )
  (testing "evaluar-de función tiene dos elementos (el segundo es un símbolo)"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de f) '(x 1))))
  )
  (testing "evaluar-de tercer elemento de función no es un símbolo"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2) '(x 1))))
  )
  (testing "evaluar-de tercer y cuarto elemento de función no son símbolos"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2 3) '(x 1))))
  )
  (testing "evaluar-de tiene dos elementos (el segundo es una lista)"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de (f)) '(x 1))))
  )
  (testing "evaluar-de tercer elemento es un símbolo"
    (is (= '((*error* list expected x) (x 1)) (evaluar-de '(de 2 x) '(x 1))))
  )
  (testing "evaluar-de segundo elemento no es un símbolo"
    (is (= '((*error* symbol expected 2) (x 1)) (evaluar-de '(de 2 (x)) '(x 1))))
  )
  (testing "evaluar-de el segundo elemento es nil"
    (is (= '((*error* cannot-set nil) (x 1)) (evaluar-de '(de nil (x) 2) '(x 1))))
  )
)

; tests de evaluar-if

; importante: en los mensajes de testing sólo se aclara lo que se retorna como primer elemento
; del resultado ya que el segundo elemento es siempre el ambiente actualizado.

(deftest evaluar-if-tests
  (testing "evaluar-if con único elemento t retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con único elemento dígito retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con único elemento nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con único elemento clave existente retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos (clave t existente en ambiente global y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos (clave z existente en ambiente local y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos (clave z en mayúscula existente en ambiente local y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if Z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos (clave w existente en ambiente global y un valor) retorna el valor recibido"
    (is (= '(9 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos (clave inexistente) retorna error"
    (is (= '((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con dos elementos y primer elemento nil retorna nil"
    (is (= '(nil (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con tres elementos y primer elemento nil retorna valor asociado a última clave existente en ambiente local"
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con seis elementos y primer elemento nil retorna valor asociado a última clave existente en ambiente local"
    (is (= '("hola" (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con tres elementos y primer elemento nil retorna valor asociado a ultima clave existente en ambiente global"
    (is (= '(3 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con tres elementos y primer elemento nil retorna último elemento"
    (is (= '(8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con tres elementos y primer elemento nil retorna último elemento"
    (is (= '((8 (nil nil t t v 1 w 3 x 6)) (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  )
  (testing "evaluar-if con tres elementos y primer elemento condición true retorna error por clave desconocida a"
    (is (= '((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"
    ))))
  )
  (testing "evaluar-if con tres elementos y primer elemento condición nil retorna último elemento"
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6)) (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
  (testing "evaluar-if con tres elementos y primer elemento condición nil retorna primer elemento del resultado de evaluar la expresión del último resultado"
    (is (= '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8)) (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola"))))
  )
)

; tests de evaluar-or

; importante: en los mensajes de testing sólo se aclara lo que se retorna como primer elemento
; del resultado ya que el segundo elemento es siempre un ambiente.

(deftest evaluar-or-tests
  (testing "evaluar-or sin expresión devuelve nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con or en mayúscula sin expresión devuelve nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(OR) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento nil devuelve nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento nil literal devuelve nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or 'NIL) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento cadena vacía devuelve nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or '()) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento booleano t devuelve t"
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento booleano t en mayúscula devuelve t"
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or T) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento clave existente en ambiente global devuelve valor asociado"
    (is (= '(5 (nil nil t t w 5 x 4)) (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento clave existente en mayúscula en ambiente global devuelve valor asociado"
    (is (= '(5 (nil nil t t w 5 x 4)) (evaluar-or '(or W) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento clave inexistente devuelve error"
    (is (= '((*error* unbound-symbol r) (nil nil t t w 5 x 4)) (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento clave existente en ambiente global devuelve valor asociado nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con único elemento dígito lo retorna"
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con nil y dígito retorna dígito"
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con primer elemento expresión con llamado a setq retorna primer elemento del resultado de evaluar la expresión"
    (is (= '(8 (nil nil t t w 5 x 4 b 8)) (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con dos nil y un dígito retorna dígito"
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con múltiples nil y expresión nil retorna dígito"
    (is (= '(6 (nil nil t t w 5 x 4)) (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con múltiples nil, expresión nil y t retorna t"
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con múltiples nil, expresión nil en mayúscula y t en mayúscula retorna t"
    (is (= '(t (nil nil t t w 5 x 4)) (evaluar-or '(OR nil T R nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-or con múltiples nil retorna nil"
    (is (= '(nil (nil nil t t w 5 x 4)) (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3))))
  )
)

; tests de evaluar-setq

(deftest evaluar-setq-tests
  (testing "evaluar-setq sin argumentos retorna nil"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con un solo argumento retorna nil"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con un par clave valor inexistente los añade y retorna valor con el ambiente actualizado"
    (is (= '(7 (nil nil t t + add w 5 x 4 m 7)) (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con un par clave valor inexistente los añade y retorna valor con el ambiente actualizado con 'setq' en mayúscula"
    (is (= '(7 (nil nil t t + add w 5 x 4 m 7)) (evaluar-setq '(SETQ m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con una clave existente y un nuevo valor retorna el nuevo valor y el ambiente actualizado"
    (is (= '(7 (nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con una clave existente en ambos ambientes cuyo valor debe ser calculado utiliza valor del ambiente local para evaluar y actualizar"
    (is (= '(2 (nil nil t t + add w 5 x 2)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con una clave existente solo en el ambiente global cuyo valor debe ser calculado utiliza valor del ambiente global para evaluar y actualizar"
    (is (= '(5 (nil nil t t + add w 5 x 5)) (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
  (testing "evaluar-setq con argumento nil retorna error"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con argumento nil literal retorna error"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq 'NIL) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con argumento lista vacía retorna error"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq '()) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con clave nil retorna error"
    (is (= '((*error* cannot-set nil) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con clave no símbolo retorna error"
    (is (= '((*error* symbol expected 7) (nil nil t t + add w 5 x 4)) (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq con dos claves, donde el valor de la segunda clave debe ser calculado utiliza valor primera clave del ambiente local para ejecutar el cálculo"
    (is (= '(8 (nil nil t t + add w 5 x 7 m 8)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3))))
  )
  (testing "evaluar-setq utiliza valor de primera clave en ambiente global para obtener el valor a asignar a la segunda clave"
    (is (= '(14 (nil nil t t + add w 5 x 7 m 14)) (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
  (testing "evaluar-setq con dos claves pero solo un valor asignado a la primera clave retorna error"
    (is (= '((*error* list expected nil) (nil nil t t + add w 5 x 7)) (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
  (testing "evaluar-setq con tres claves retorna valor de la última clave por setear"
    (is (= '(9 (nil nil t t + add w 5 x 7 y 8 z 9)) (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
  (testing "evaluar-setq con tres claves retorna valor de la última clave por setear pero en mayúscula"
    (is (= '(9 (nil nil t t + add w 5 x 7 y 8 z 9)) (evaluar-setq '(setq X 7 Y 8 Z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3))))
  )
)
