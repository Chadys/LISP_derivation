# LISP_derivation
Open with "DrRacket" and use the function "deriver" with the function you want to derivate in a list as argument like this :
> (deriver '(/ (+ (* 6 (expt x 3)) (+ (expt x 2) x)) (- (* 3 x) 3))))
'(/ (- (+ (* 6 (expt x 3)) (+ (expt x 2) 1)) (- (* 3 x) 3)) (expt (- (* 3 x) 3) 2))
You will obtain the list representing your function once derivated.


To calculate the result of a derivated function with different 'x', use the program this way :
> (crefonction 'fonction '(+ (* 2 x) (expt x 2)))
> (fonction 7)
16
> (fonction 9)
20
> (fonction 83)
168
"crefonction" create a function named by its first argument by derivating its second argument.
You will then be able to use that derivated function that will give you the result of its formula by replacing 'x' with the value you want.
