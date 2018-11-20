(defun findfactor (term) 
  (car term))

(defun findpower (term)
  (cadr term))

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



;;samples of recursive execution
;;(add-poly `((3 3)(2 2)(1 1)) (add-poly `((1 5)) `((2 5))))
;;(add-poly `((3 3)(2 2)(1 1)) (subtract-poly `((1 5)) `((2 5)(-2 2))))