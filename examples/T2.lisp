(defmethod make-rule ((play) &aux if-part then-part)
   (setf if-part (list 'prefix 'of play 'matches 'the 'play 'so 'far))
   (setf then-part (lst 'select 'move 'from play))
   (list 'if if-part 'then then-part)
)

(defmethod matches ((psf list) (play list))
   (cond
      ((null psf) t)
      ((eq (car psf) (car play)) (matches (cdr psf) (cdr play)))
      (t nil)
   )
)

(defclass h-m-p (player)
   (
      (rules :accessor h-m-p-r:initform())
   )
)
