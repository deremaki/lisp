(defun findfactor (term)
  (cond ((listp term)
          (car term))
        ((not (listp term))
          term
        )
  )
  )

(defun findpower (term)
  (cond ((listp term)
          (cadr term))
        ((not (listp term))
          term
        )
  )
  )

(defun maketerm (factor power)
  (list factor power))

(defun print-poly (poly)
   (mapcar #'print-term poly)
  )

(defun print-term (term)
    (cond ((< (findfactor term) 0)
              (princ " - "))
          ((>= (findfactor term) 0)
              (princ " + "))
    )
    (cond ((not (= (abs (findfactor term)) 1))
         (princ (abs (findfactor term))))
    )
    (princ "x")
    (cond ((not (= (findpower term) 1))
         (princ "^")
         (princ (findpower term))
         )
    )
  )

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

(defun subtract-poly (poly1 poly2)
  (let ((term1 (car poly1))
        (term2 (car poly2)))
    (cond ((null term1) poly2)
          ((null term2) poly1)
          ((= (findpower term1) (findpower term2))
              (cons (maketerm (- (findfactor term1) (findfactor term2)) (findpower term1))
                    (subtract-poly (rest poly1) (rest poly2))))
          ((> (findpower term1) (findpower term2))
           (cons term1 (subtract-poly (rest poly1) poly2)))
          ((< (findpower term1) (findpower term2))
           (cons term2 (subtract-poly poly1 (rest poly2)))))))

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

(defun div-poly(poly1 poly2)
;;division of polynomials with remainder
;;example usage: (div-poly `((1 3) (-2 2) (-4 0)) `((1 1) (-3 0))) -> ((3 1) (6 2) (0 0))
  (let (temp `() ))
  (let (q `() ))
  (let (r `() ))
  (setf q `((0 0)) )
  (setf r poly1)
  ;;(loop while (and (not (equal r `() )) (>= (degree r) (degree poly2)) ) 
  (loop :for term in poly1 
      :do 
        (setf temp (div-term (lead r) (lead poly2) ))
        (setf q (add-poly q temp))
        (setf r (subtract-poly r (mul-poly temp poly2)))
      :finally (return q)
    )
  )

(defun div-term (term1 term2)
;;simple division of single terms
;;example usage: (div-term `(4 2) `(2 1)) -> (2 1)
  (maketerm (/ (findfactor term1) (findfactor term2)) (- (findpower term1) (findpower term2) ))
)

(defun degree(poly)
;;returns the degree of polynomial
;;example usage: (degree `((3 4)(8 2)(6 0))) -> 4
  (loop
      :with deg = -1
      :for term in poly
      :do (cond ((> (findpower term) deg)
            (setf deg (findpower term))))
      :finally (return deg)
  )
)

(defun lead(poly)
;;returns leading term of polynomial
;;example usage: (term `((3 4)(8 2)(6 0))) -> (3 4)
  (car poly)
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
    (cons (list (/ factor (+ power 1)) (+ power 1)) (integral-poly (rest poly))))
    )
    ))

(defun calculate-poly(poly x)
  (loop :with result = 0
        :for i :from (1- (length poly)) :downto 0
        :do (setf result (+ (findfactor (nth i poly)) (* result x)))
        :finally (return result))
  )

(print (diff-poly '((4 -5)(-6 2)(3 1)(5 0))))

(print (integral-poly '((7 6)(3 4)(1 2)(1 1))))

;;(print (power-poly '((3 3)(2 2)(1 1)(4 4)) '(5 2)))
 
;(print (power-poly '((3 5)) '(1 2)))
; http://matematyka.pisz.pl/strona/1434.html
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


;samples of recursive execution
;(add-poly `((3 3)(2 2)(1 1)) (add-poly `((1 5)) `((2 5))))
;(add-poly `((3 3)(2 2)(1 1)) (subtract-poly `((1 5)) `((2 5)(-2 2))))(defun mul-poly(poly1 poly2)
;(calculate-poly `((3 4)(8 2)(6 0)) 3)



(defmacro int-diff (poly r-factor fun-factor r-power fun-power)
  (let ((power (findpower (car poly)))
        (factor (findfactor (car poly)))
        (rt (rest poly)))
    (cond ((null (rest poly))   
            `(list (list (funcall #',fun-factor (,@power) (,@factor)) 
                              (funcall #',fun-power  (,@power) (,@factor))))
            )
    (T 
            ;(let ((tmp `(,@power))))e
            `(cons  (list (funcall #',fun-factor (,@power) (,@factor)) 
                              (funcall #',fun-power  (,@power) (,@factor)))
                   (int-diff ,rt r-factor ,fun-factor r-power ,fun-power)
                ;  (print `(,@power))
              ;     `(if-let (,@rt) r-factor fun-factor r-power fun-power)
                   )
            )
    )
    ))

(print   (int-diff ((4 6)(2 2)(8 8)) 
            r-factor 
            (lambda (power factor) (/ factor (+ power 1)))
            r-power
            (lambda (power factor) (+ power 1))
            ))


(print (macroexpand-1 `(int-diff ((4 6)(2 2)(8 8)(1 1)(4 4)) 
            r-factor 
            (lambda (power factor) (/ factor (+ power 1)))
            r-power
            (lambda (power factor) (+ power 1))
            )))

(defun integral-poly(poly)
     (eval `(int-diff ,poly 
            r-factor 
            (lambda (power factor) (/ factor (+ power 1)))
            r-power
            (lambda (power factor) (+ power 1))
            )))  
            
(defun diff-poly(poly)
     (eval `(int-diff ,poly 
            r-factor 
            (lambda (power factor) (* factor power))
            r-power
            (lambda (power factor) (- power 1))
            )))        
            

(print (diff-poly '((4 -5)(-6 2)(3 1)(5 0))))

(print (integral-poly '((7 6)(3 4)(1 2)(1 1))))



(defmacro p (a x^ b &rest values)         
    (let ((fst (nth 0 values)) 
    (th (nth 2 values))
    (rest-val (nthcdr 3 values)))
    (cond ((null values)   `(list (list ,a ,b)))
          (T `(list (list ,a ,b) (p ,fst x^ ,th ,@rest-val)  )))
)    
)
(print (p 4 x^ 3 8 x^ 2 9 x^ 3))
(print (p 4 x^ 3 ))

(print (diff-poly (p 4 x^ 3 )))