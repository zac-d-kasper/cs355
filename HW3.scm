;Zac Kasper
;Cpts 355, Spring 2017
;Assignment 3

;FUNCTION 1
(define (deepSum L) (cond
                      ((null? L) 0)
                      ((pair? (car L)) (+ (deepSum (car L)) (deepSum(cdr L))))
                      ; if element is sublist, sum up sublist and cdr of current list
                      (else (+ (car L) (deepSum (cdr L))))
                      ))
(define (testDeepSum) (cond
                          ((eqv? 66 (deepSum '(1 (2 3 4) (5) 6 7 (8 9 10) 11))) (display "deepSum succeeded \n"))
                          (else (display "deepSum failed \n"))
                          ))
(testDeepSum)


;FUNCTION 2
(define (numbersToSum sum L) (cond
                               ((<= (- sum (car L)) 0) '())  
                               (else (cons (car L) (numbersToSum (- sum (car L)) (cdr L))))
                               ;uses sum as decrementing limit
                               ))
(define (testNumToSum) (cond
                         ((equal? '(5 4 6 10 4) (numbersToSum 30 '(5 4 6 10 4 2 1 5))) (display "numbersToSum succeeded \n"))
                         (else (display "numbersToSum failed \n"))
                         ))
(testNumToSum)


;FUNCTION 3
(define (isSorted L) (cond
                       ((null? (cdr L)) #t)
                       ; if L is length 1, must be true
                       ((> (car L) (car (cdr L))) #f)
                       ; compares current and next values, if curr > next, return false 
                       (else (isSorted (cdr L)))
                       ))
(define (testIsSorted) (cond
                         ((eqv? #t (isSorted '(1 4 5 6 10))) (display "isSorted succeeded \n"))
                         (else (display "isSorted failed \n"))
                         ))
(testIsSorted)


;FUNCTION 4
(define (mergeUnique2 L1 L2) (cond
                               ((null? L1) L2)
                               ((null? L2) L1)
                               ((= (car L1) (car L2)) (cons (car L1) (mergeUnique2 (cdr L1) (cdr L2))))
                               ; if elements are eqv, cons said element, decrement both lists
                               ((> (car L1) (car L2)) (cons (car L2) (mergeUnique2 L1 (cdr L2))))
                               ; if L1[0] > L2[0], cons L2[0], and decrement L2
                               (else (cons (car L1) (mergeUnique2 (cdr L1) L2)))
                               ; if L1[0] < L2[0], cons L1[0], decrement L1
                               ))
(define (tMergeUnique2) (cond
                          ((equal? '(1 2 5 7) (mergeUnique2 '(1 5 7) '(2 5 7))) (display "mergeUnique2 succeeded \n"))
                          (else (display "mergeUnique2 failed \n"))
                          ))
(tMergeUnique2)


(define (fold f base L) (cond
                          ((null? L) base)
                          (else (f (car L) (fold f base (cdr L))))
                          ))
(define (mymap f L) (cond
                      ((null? L) '())
                      (else (cons (f (car L)) (mymap f (cdr L))))
                      ))


;FUNCTION 5
(define (mergeUniqueN Ln) (fold mergeUnique2 '() Ln))
(define (testMergeN) (cond
                       ((equal? '(1 2 3 4 5 6 9) (mergeUniqueN '((2 4 6) (1 4 5 6) (3 9)))) (display "mergeUniqueN succeeded \n"))
                       (else (display "mergeUniqueN failed \n"))
                       ))
(testMergeN)
;for mergeUnique2 it requires (len1 + len2) cons operations, one for each element
;for mergeUniqueN it requires sum[(n - 1) * (len_i + len_i+1)] cons operations
;two lists are merged into one, then repeats for n-1 times
;greatly increases the number of cons operations with large numbers of sublists 


;FUNCTION 6
(define (matrixMap f M) (mymap (lambda (sL) (mymap f sL)) M))
(define (testMatrixMap) (cond 
                          ((equal? '((1 4) (9 16)) (matrixmap (lambda (x) (* x x)) '((1 2) (3 4)))) (display "matrixMap succeeded \n"))
                          (else (display "matrixMap failed \n"))
                          ))
(testMatrixMap)


;helper for avgOdd
;takes list of ints, returns list with only odd values
(define (getOdds L) (cond
                      ((null? L) '())
                      ((odd? (car L)) (cons (car L) (getOdds (cdr L))))
                      (else (getOdds (cdr L)))
                      ))
;FUNCTION 7
(define (avgOdd L) (/ (fold + 0 (getOdds L))) (length (getOdds L)))
(define (testAvgOdd) (cond
                       ((eqv? 3 (avgOdd '(1 2 3 4 5))) (display "avgOdd succeeded \n"))
                       (else (display "avgOdd failed \n"))
                       ))
(testAvgOdd)

;makes list from first element in each sublist
(define (skimMerge L) (mymap (lambda (sL) (car sL)) L))
;makes list from second element in each sublist
(define (shaveMerge L) (mymap (lambda (sL) (car (cdr sL))) L))
;FUNCTION 8
(define (unzip L) (cond
                    ((null? L) '())
                    (else (cons (skimMerge L) (cons (shaveMerge L) '())))
                    ))
(define (testUnzip) (cond
                      ((equal? '((1 5 8) ("a" "b" "c")) (unzip '((1 "a") (5 "b") (8 "c")))) (display "unzip succeeded \n"))
                      (else (display "unzip failed \n"))
                      ))
(testUnzip)