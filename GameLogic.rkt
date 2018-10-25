#lang racket

 ;;;; GRAN PACK DE VERIFICACIONES ;;;;;
;;Made and Compilated by Lord Ricardo Molina
;verificar victoria
(define (win? matriz num) ;Verifica si se cumple la condición de victoria
  (or (horizontal matriz num) (vertical matriz num) ;Se gana alineando el min requerido en Horizontal, Verticar o Diagonal con un OR
       (diagonales matriz num))
  )

;Operación Básica
;transpone la matriz
(define (transpuesta matriz) 
  (cond((null? matriz)
        '() ; COND DE PARADA
        )
       ((null? (car matriz))
        '()
        ) ; REGRESA NULO
       (else( cons (get_column matriz) (transpuesta (quit_col matriz))))
   ))

;Técnica de análisis
;elimina primera columna
(define (quit_col matriz)
  (cond((null? matriz)
        '() ;COND DE PARADA
        )
       (else(cons (cdar matriz) (quit_col (cdr matriz))))
  ))

;Paso final
;verifica si hay una fila llena de num (Caso 1, fila llena)
(define (horizontal matriz num)
  (cond((null? matriz)#f)
        ((linea_equal (car matriz) num) #t) ;IGUAL? DEVUELVA TRUE Y GANASTES
        (else(horizontal (cdr matriz) num))
   ))

;Paso final
;verifica si hay una columna llena de num (Caso 2, columna llena)
(define (vertical matriz num)
  (horizontal (transpuesta matriz) num) ;MÁS FÁCIL TRANSPONER Y HACER EL ANÁLISIS HORIZONTAL
  )

;Operación básica
;obtener la primera columna de la matriz
(define (get_column matriz)
  (cond((null? matriz)
        '() ;COND DE PARADA
        )
       (else(cons (caar matriz) (get_column (cdr matriz))))
   ))

;Caso Base
;verifica si todos los elementos en la lista son iguales a num (Caso Base)
(define (linea_equal lista num)
  (cond((null? lista)#t);SI LLEGA A SER NULO, TODOS LOS NODOS SON IGUALES
       ((equal? (car lista) num) (linea_equal (cdr lista) num)) ;SI ES IGUAL, LO DESCARTA
       (else #f)
  ))

;Operación básica
;Invierte la lista o voltea la matriz horizontalmente
(define (invertir lista )
  (cond((null? lista) '())
       (else( append (invertir (cdr lista))  (list(car lista)) ))
   ))

;Verificación final
;verifica solo una diagonal de primer tipo
;\
(define (diagonal matriz num)
  (cond((null? matriz)#t)
       ((null? (car matriz)) #t) ;COND SALVATANDAS("MANO DE DIOS")
       ((and (equal? (caar matriz) num) (not(null? (cdar matriz))) (null? (cdr matriz)) ) #f)
       ((equal? (caar matriz) num) (diagonal (cdr(quit_col matriz)) num) )
       (else #f)
   ))

;Verificación final
;verificar diagonales de derecha inferior a izquierda superior
; \
(define (diag_aux matriz num)
  (cond ((null? matriz)
         #f
         );FILTRA
        ((diagonal matriz num)
         #t
         )
        (else (diag_aux (cdr matriz) num));QUITO LA PRIMERA LISTA Y SIGO ANALIZANDO
  ))

;Verificación final
;Revisa las diagonales de derecha superior a izquierda inferior
;/
(define (diagonal2 matriz num)
  (diag_aux (invertir matriz) num);MÁS FÁCIL INVERTIR LA MATRIZ Y USAR EL ALGORITMO DIAG_AUX
)

(define (diagonales matriz num)
  (or (diag_aux matriz num) (diagonal2 matriz num)
      (diag_aux (transpuesta matriz) num) (diagonal2 (transpuesta matriz) num)));Pasa dándole la vuelta, reutilizando solo un algoritmo de recorrido

(provide (all-defined-out))