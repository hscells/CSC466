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

(defmethod visualize ((lst list))
   (setf grid '(nil nil nil nil nil nil nil nil nil))
   (setf board '(nw n ne w c e sw s se))
   (loop for i from 0 upto (length lst) do
      (setf move (position (nth i board) lst :test #'equal))
      (cond
         ((not (eq move nil)) (setf (nth i grid) move))
      )
   )
   (setf c 0)
   (dolist (g grid)
      (cond
         ((eq g nil) (format t " -- "))
         (t (format t " ~S~S " (playern g) (+ g 1)))
      )
      (cond
         ((eq c 2) (format t "~%"))
         ((eq c 5) (format t "~%"))
      )
      (setf c (+ c 1))
   )
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

; root of the player hierarchy
(defclass player()
   ((name :accessor player-name :initarg :name :initform 'emanon))
)

; random machine player
(defclass random-machine-player (player) ())

; a human player
(defclass human-player (player) ())

; textual display of a random machine player
(defmethod display ((p random-machine-player))
   (format t "RANDOM MACHINE PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
)
; textual display of a human player
(defmethod display ((p human-player))
   (format t "HUMAN PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
)

; move making method for machine
(defmethod make-move ((p random-machine-player) (report t) &aux move)
   (if report (format t "BEGIN RANDOM PLAYER MOVE ...~%"))
   (setf move (select *avail*))
   (if report (format t "randomly selecting ~A for my move ~%" move))
   (setf *avail* (remove move *avail*))
   (if report (format t "END RANDOM PLAYER MOVE ...~%"))
   move
)

; move making method for human
(defmethod make-move ((p human-player) (report t) &aux move)
   (if report (format t "BEGIN HUMAN PLAYER MOVE ... ~%"))
   (format t "Please select a move from ~A~%" *avail*)
   (setf move (read))
   (cond
      ((not (member move *avail*))
         (make-move p)
      )
      (t
         (setf *avail* (remove move *avail*))
         move
      )
   )
   (if report (format t "END HUMAN PLAYER MOVE~%"))
   move
)

; a generic play method
(defmethod generic-play ((x player) (o player) (report t) &aux move)
   (setf *avail* '(nw n ne w c e sw s se))
   (setf *play-so-far* ())
   (dolist (player '(x o x o x o x o x))
      (if (or report (equal (type-of o) 'human-player-machine))
       (visualize *play-so-far*)
      )
      (cond
         ((eq player 'x)
            (setf move (make-move x report))
         )
         ((eq player 'o)
            (setf move (make-move o report))
         )
      )
      (setf *play-so-far* (snoc move *play-so-far*))
      (if (game-over-p *play-so-far*) (return nil))
   )
   *play-so-far*
)

; predicate to determine if the play is over or not
(defmethod game-over-p ((play list))
   (cond
      ((line-p (odd play)) 'w)
      ((line-p (even play)) 'l)
      ((= (length play) 9) 'd)
      (t nil)
   )
)

(defmethod odd ((l list))
   (cond
      ((null l) ())
      ((null (cdr l)) (list (car l)))
      (t (cons (car l) (odd (cddr l))))
   )
)

(defmethod even ((l list))
   (cond
      ((null l) ())
      ((null (cdr l)) ())
      (t (cons (cadr l) (even (cddr l))))
   )
)

(defun line (a b c)
   (setf l (list a b c))
   (setf wcond '(
      (nw n ne) (e c w) (sw s se)
      (nw w sw) (n c s) (ne e se)
      (ne c sw) (nw c se)
   ))
   (dolist (w wcond)
      (cond
         ((eq (length (intersection w l)) 3)
            t
         )
      )
   )
   nil
)

; TODO
; What does this (line x y z) method mean?
(defmethod line-p ((l list))
   (cond
      ((< (length l) 3)
         nil
      )
      ((= (length l) 3)
         (line (first l) (second l) (third l))
      )
      (( = (length l) 4)
         (or
            (line (first l) (second l) (third l))
            (line (first l) (second l) (fourth l))
            (line (first l) (third l) (fourth l))
            (line (second l) (third l) (fourth l))
         )
      )
      ((= (length l) 5)
         (or
            (line (first l) (second l) (third l))
            (line (first l) (second l) (fourth l))
            (line (first l) (second l) (fifth l))
            (line (first l) (third l) (fourth l))
            (line (first l) (third l) (fifth l))
            (line (first l) (fourth l) (fifth l))
            (line (second l) (third l) (fourth l))
            (line (second l) (third l) (fifth l))
            (line (second l) (fourth l) (fifth l))
            (line (third l) (fourth l) (fifth l))
         )
      )
   )
)

; two random players play one game
(defmethod demo-random-random (&aux p x o)
   (setf x (make-instance 'random-machine-player))
   (setf o (make-instance 'random-machine-player))
   (setf p (generic-play x o t))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)

; a random machine player and a human play one game
(defmethod demo-random-human (&aux p x o)
   (setf x (make-instance 'random-machine-player))
   (setf o (make-instance 'human-player))
   (setf p (generic-play x o t))
   (format t "~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   nil
)
