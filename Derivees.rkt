#lang racket

#|---------------- Fonctions utiles -----------------|#

(define (liste a b)
  (cons a (cons b '())))

(define (liste3 a b c)
  (cons a (cons b (cons c '()))))

(define (-cos l)
  (liste3 '* -1 (liste 'cos l)))

(define (-sin l)
  (liste3 '* -1 (liste 'sin l)))

(define (o l m)
  (remplacex l m))

(define (remplacex l n)
  (if (pair? l)
        (cons (remplacex (car l) n) (remplacex (cdr l) n))
        (if (equal? l 'x)
            n
            l)))


#|------------- Derivees au cas par cas -------------|#

(define (derivepow l)
  (liste3 '* (liste3 '* (car (cdr (cdr l))) (liste3 'expt (car (cdr l)) (- (car (cdr (cdr l))) 1))) (derive (car (cdr l)))))

(define (deriveadd l)
      (liste3 '+ (derive (car (cdr l))) (derive (car (cdr (cdr l))))))

(define (derivesous l)
      (liste3 '- (derive (car (cdr l))) (derive (car (cdr (cdr l))))))

(define (derivemult l)
      (liste3 '+ (liste3 '* (derive (car (cdr l))) (car (cdr (cdr l)))) (liste3 '* (car (cdr l)) (derive (car (cdr (cdr l)))))))

(define (derivediv l)
  (liste3 '/ (liste3 '- (derive (car (cdr l))) (derive (car (cdr (cdr l))))) (liste3 'expt (car (cdr (cdr l))) 2)))

(define (derivesqrt l)
  (liste3 '/ (derive (car (cdr l))) (liste3 '* 2 (liste 'sqrt (car (cdr l))))))

(define (derivecomp l)
  (liste3 '* (liste3 'o (derive (car (cdr l))) (car (cdr (cdr l)))) (derive (car (cdr (cdr l))))))

(define (derivereciproq l)
  (liste3 '/ 1 (liste3 'o (derive (car (cdr l))) (liste3 'expt (car (cdr l)) -1))))

(define (derivecos l)
  (-sin (car (cdr l))))

(define (derivesin l)
  (liste 'cos (car (cdr l))))

(define (derive-cos l)
  (liste 'sin (car (cdr l))))

(define (derive-sin l)
  (-cos (car (cdr l))))

(define (derivetan l)
  (derive '(/ (sin x) (cos x))))

(define (deriveexpo l)
  (liste3 '* (liste3 'expt 'exp (car (cdr (cdr l)))) (derive (car (cdr (cdr l))))))

(define (deriveln l)
  (liste3 '/ (derive (car (cdr l))) (car (cdr l))))


#|---------------- Derivees generales ----------------|#

(define (derive x)
  (if (pair? x)
      (if (equal? (car x) 'expt)
          (if (equal? (car (cdr (cdr x))) -1)
              (derivereciproq x)
              (if (equal? (car (cdr x)) 'exp)
                  (deriveexpo x)
                  (derivepow x)))
          (if (equal? (car x) '+)
              (deriveadd x)
              (if (equal? (car x) '-)
                  (derivesous x)
                  (if (equal? (car x) '*)
                      (derivemult x)
                      (if (equal? (car x) '/)
                          (derivediv x)
                          (if (equal? (car x) 'sqrt)
                              (derivesqrt x)
                              (if (equal? (car x) 'o)
                                  (derivecomp x)
                                  (if (equal? (car x) 'cos)
                                      (derivecos x)
                                      (if (equal? (car x) '-cos)
                                          (derive-cos x)
                                          (if (equal? (car x) 'sin)
                                              (derivesin x)
                                              (if (equal? (car x) '-sin)
                                                  (derive-sin x)
                                                  (if (equal? (car x) 'tan)
                                                      (derivetan x)
                                                          (if (equal? (car x) 'log)
                                                              (deriveln x)
                                                              x)))))))))))))
      (if (equal? x 'x)
          1
          0)))


#|----------- Simplification de fonctions -----------|#

(define (effaceinutile l)
  (if (pair? l)
      (if (equal? (car l) '*)
          (if (equal? (car (cdr l)) 0)
              0
              (if (equal? (car (cdr (cdr l))) 0)
                  0
                  (if (equal? (car (cdr l)) 1)
                      (car (cdr (cdr l)))
                      (if (equal? (car (cdr (cdr l))) 1)
                          (car (cdr l))
                          l))))
          (if (equal? (car l) '+)
              (if (equal? (car (cdr l)) 0)
                  (car (cdr (cdr l)))
                  (if (equal? (car (cdr (cdr l))) 0)
                      (car (cdr l))
                      l))
              (if (equal? (car l) 'expt)
                  (if (equal? (car (cdr (cdr l))) 0)
                      1
                      (if (equal? (car (cdr (cdr l))) 1)
                          (car (cdr l))
                          l))
                  (if (equal? (car l) 'o)
                      (if (pair? (car (cdr l)))
                          l
                          (if (equal? (car (cdr l)) 'x)
                              (car (cdr (cdr l)))
                              (car (cdr l))))    
                      l))))
                  l))

(define (simplifie l)
  (if (pair? l)
        (effaceinutile (cons (simplifie (car l)) (simplifie (cdr l))))
        l))

(define (deriver l)
  (simplifie (derive l)))


#|---------- Calcul de la fonction derivee ----------|#

(define (listf nom fonction)
  (liste3 'define (liste nom 'x) (derive fonction)))

(define (crefonction nom fonction)
  (eval (listf nom fonction)))