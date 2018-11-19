(defun findfactor (term) 
  (car term))

(defun findpower (term)
  (cadr term))

(defun maketerm(factor power)
  (list factor power))

(defun add-poly (poly1 poly2)
  (let ((term1 (car poly1))
        (term2 (car poly2)))
    (cond ((null term1) poly2)
          ((null term2) poly1)
          ((= (findpower term1) (findpower term2))
              (cons (maketerm (+ (findfactor term1) (findfactor term2)) (findpower term1))
                    (add-poly (rest poly1) (rest poly2))))
          ((> (findpower term1) (findpower term2))
           (cons term1 (add-poly (rest poly1) poly2)))
          ((< (findpower term1) (findpower term2))
           (cons term2 (add-poly poly1 (rest poly2)))))))
(defun power-poly(poly term)
    (let ((power  (findpower  (car poly))) 
          (factor (findfactor (car poly)))
          (f (findfactor term))
          (pow (findpower term)))
    (cond ((null (rest poly))   (list (list (* f factor) (+ power pow))))
    (T 
    (cons (list (* f factor) (+ power pow)) (power-poly (rest poly) term)))
    )
    ))

(defun mul-poly(poly1 poly2)
    (let ((term (car poly2)))
    (cond
        ((null (rest poly2))  (power-poly poly1 term))
        (T
        
   (add-poly (power-poly poly1 term) (mul-poly poly1 (rest poly2)))
  ;  (mul-poly poly1 (rest poly2))
    ))
    )
)


(defun diff-poly(poly)
    (let ((power  (findpower  (car poly))) 
          (factor (findfactor (car poly))))
    (cond ((null (rest poly))   (list (list (* power factor) (- power 1))))
    (T 
    (cons (list (* power factor) (- power 1)) (diff-poly (rest poly))))
    )
    ))
(defun integral-poly(poly)
    (let ((power  (findpower  (car poly))) 
          (factor (findfactor (car poly))))
    (cond ((null (rest poly))   (list (list (/ factor (+ power 1) ) (+ power 1))))
    (T 
    (cons (list (/ factor (+ power 1)) (+ power 1)) (diff-poly (rest poly))))
    )
    ))

(print (diff-poly '((4 -5)(-6 2)(3 1)(5 0))))

(print (integral-poly '((7 6)(3 2)(1 0)(1 -1))))

;;(print (power-poly '((3 3)(2 2)(1 1)(4 4)) '(5 2)))
 
;(print (power-poly '((3 5)) '(1 2)))
 http://matematyka.pisz.pl/strona/1434.html
(print (mul-poly '((4 5)(-6 2)(3 1)) '((5 4)(-2 2)(3 0))))
 
;;(print (mul-poly '((4 5)) '((1 1))))


;;sample recursive execution
;print  (add-poly `((3 3)(2 2)(1 1)) `((3 3)(2 2)(1 1)) ))

;;(print (add-poly `((1 5)) `((2 5))))
(defmacro eight (a b) 
  (add-poly a 
    (add-poly a b)))

;(print (eight '((1 5)) '((2 5))))
;(add-poly `((1 5)) `((2 5))))

;(print (macroexpand-1 '(eight ((1 5)) ((2 5)))))

