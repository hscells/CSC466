;SELECT
(defun select (lst)
   (nth (random (length lst)) lst)
)

;SNOC
(defun snoc (obj lst)
   (append lst (list obj))
)

;PLAY
(defmethod play (&aux play avail move)
   (setf play())
   (setf avail '(nw n ne w c e sw s se))
   (dolist (player '(x o x o x o x o x))
      (cond
         ((eq player 'x)
            (setf move (select avail))
            (setf avail (remove move avail))
            (setf play (snoc move play))
         )
         ((eq player 'o)
            (setf move (select avail))
            (setf avail (remove move avail))
            (setf play (snoc move play))
         )
      )
   )
   play
)

; get the position of player in a game
(defun pos (obj lst)
  (loop for i in lst and position from 0 when (eql i obj) return position)
)

; get the player name from the play made
(defun playern (num)
   (cond ((eq 0 (mod num 2)) 'X)(t 'O))
)

; draws a single row of the board
(defun drawrow (moves lst)
   (dolist (move moves)
      (format t "~S~S " (playern(pos move lst)) (+ 1 (pos move lst)))
   )
)

(defun visualize (lst)
   (drawrow '(nw n ne) lst)
   (format t "~%")
   (drawrow '(w c e) lst)
   (format t "~%")
   (drawrow '(sw s se) lst)
   (format t "~%")
)

; find the highest score for a given 'winning' outcome
(defun find-highest (play lst)
   (setf highest 0)
   (dolist (n play)
      (cond
         ((> (pos n lst) highest) (setf highest (pos n lst)))
      )
   )
   (return-from find-highest highest)
)

; analyse the final play
(defun analyze (lst)
   ; extract out each players moves
   (setf playo (list (nth 1 lst) (nth 3 lst) (nth 5 lst) (nth 7 lst)))
   (setf playx (list (nth 0 lst) (nth 2 lst) (nth 4 lst) (nth 6 lst) (nth 8 lst)))

   ; get a list of all the winning conditions
   (setf wcond '(
      (nw n ne) (e c w) (sw s se)
      (nw w sw) (n c s) (ne e se)
      (ne c sw) (nw c se)
   ))

   ; allow wins to be stored in a list
   (setf px '())
   (setf po '())
   ; move through the winning conditions and find the best play
   (dolist (w wcond)
      (cond
         ((eq (length (intersection w playx)) 3) (setf px w))
         ((eq (length (intersection w playo)) 3) (setf po w))
      )
   )
   (setf highestx (find-highest px lst))
   (setf highesto (find-highest po lst))
   ; determine the winner or if the game is a draw
   (cond
      ((and (eq px nil) (eq po nil)) (return-from analyze 'd))
      ((eq px nil) (return-from analyze 'l))
      ((eq po nil) (return-from analyze 'w))
      ((> highestx highesto) (return-from analyze 'l))
      ((< highestx highesto) (return-from analyze 'w))
   )
   (return-from analyze '?)
)

; DEMO VA
(defmethod demo-va (&aux p)
   (setf p (play))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)

; STATS
(defmethod stats ((n number) (demo t) &aux w l d)
   (if demo (format t "Begin gathering statistics ...~%"))
   (setf w 0 l 0 d 0)
   (dotimes (i n)
      (setf p (play))
      (if demo (format t "~A~%" p))
      (if demo (visualize p))
      (setf result (analyze p))
      (cond
         ((eq result 'w) (setf w (+ w 1)))
         ((eq result 'l) (setf l (+ l 1)))
         ((eq result 'd) (setf d (+ d 1)))
      )
   )
   (setf results (mapcar #'probability (list w l d) (list n n n)))
   (if demo (format t "End gathering statistics~%"))
   (mapcar #'list '(w l d) results)
)

(defmethod probability ((special integer) (total integer))
   ( / (float special) (float total))
)
