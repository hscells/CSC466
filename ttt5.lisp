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
   (setf grid (list 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil 'nil))
   (setf board '(nw n ne w c e sw s se))
   (loop for i from 0 upto (length lst) do
      (cond
         ((not (eq (position (nth i board) lst :test #'equal) nil)) (setf (nth i grid) (position (nth i board) lst :test #'equal)))
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

; heuristic machine player class
(defclass heuristic-machine-player (player)
   (
      (rules :accessor heuristic-machine-player-rules :initform ())
   )
)

; model a heuristic learning player
(defclass heuristic-learning-machine-player (heuristic-machine-player)
   ()
)

; textual display of a random machine player
(defmethod display ((p random-machine-player))
   (format t "RANDOM MACHINE PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
   nil
)
; textual display of a human player
(defmethod display ((p human-player))
   (format t "HUMAN PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "~%")
   nil
)
; textual display of a human player
(defmethod display ((p heuristic-machine-player))
   (format t "HEURISTIC MACHINE PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "number of rules = ~A~%" (length (heuristic-machine-player-rules p)))
   (format t "rules ... ~%")
   (dolist (rule (heuristic-machine-player-rules p))
      (print-rule rule) (terpri)
   )
   (format t "~%")
   nil
)

; display a heuristic learning machine
(defmethod display ((p heuristic-learning-machine-player))
   (format t "HEURISTIC LEARNING MACHINE PLAYER ...~%")
   (format t "name = ~A~%" (player-name p))
   (format t "rules ... ~%")
   (dolist (rule (heuristic-machine-player-rules p))
      (print-rule rule) (terpri)
   )
   (format t "~%")
   nil
)

(defmethod make-rule ( (l list) )
   (setf if-part (list 'prefix 'of l 'matches 'the 'play 'so 'far))
   (setf then-part (list 'select 'move 'from l))
   (list 'if if-part 'then then-part)
)

(defmethod add-rule ((p heuristic-machine-player) (l list))
   (setf
      (heuristic-machine-player-rules p)
      (append (heuristic-machine-player-rules p) (list (make-rule l)))
   )
   nil
)

(defmethod add-rules ((p heuristic-machine-player) (n integer))
   (dotimes (i n) (add-rule p (winning-play)))
   nil
)

(defmethod print-rule ((rule list))
   (format t "~A " (first rule))
   (format t "~A~%" (second rule))
   (format t "~A " (third rule))
   (format t "~A~%" (fourth rule))
   nil
)

(defmethod winning-play (&aux p)
   (setf p (play))
   (cond
      ((eq (analyze p) 'w)
         p
      )
      (t
         (winning-play))
   )
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

(defmethod make-move ((p heuristic-machine-player) (report t) &aux rule move)
   (if report (format t "BEGIN HEURISTIC PLAYER MOVE ...~%"))
   (setf rule (select-from-rule-base p))
   (if (null rule)
      (let ()
         (setf move (select *avail*))
         (setf *nr-random-moves-by-hmp* (+ 1 *nr-random-moves-by-hmp*))
         (setf *most-recent-hmp-move* 'random)
         (if report (format t "making a random move ~A since no rule is applicable.~%" move))
      )
      (let ()
         (setf move (apply-rule rule))
         (setf *nr-heuristic-moves-by-hmp* (+ 1 *nr-heuristic-moves-by-hmp*))
         (setf *most-recent-hmp-move* 'heuristic)
         (if report (format t "play so far = ~A~%" *play-so-far*))
         (if report (format t "making move ~A by applying the rule ... ~%" move))
         (if report (print-rule rule))
      )
   )
   (setf *avail* (remove move *avail*))
   (if report (format t "end heuristic player move~%"))
   move
)

(defmethod make-random-move ((p player) (report t) &aux move)
   (if report (format t "BEGIN MAKE RANDOM MOVE FOR SOME PLAYER ...~%"))
   (setf move (select *avail*))
   (if report (format t "making random move ~A~%" move))
   (setf *avail* (remove move *avail))
   (if report (format t "END MAKE RANDOM MOVE FOR SOME PLAYER ~%"))
   move
)

(defmethod select-from-rule-base ((p heuristic-machine-player) &aux rule-base)
   (setf rule-base (heuristic-machine-player-rules p))
   (dolist (rule rule-base)
      (cond
         ((applicablep rule)
            (return-from select-from-rule-base rule)
         )
      )
   )
   nil
)

(defmethod applicablep ((rule list))
   (setf the-play (third (second rule)))
   (matches *play-so-far* the-play)
)

(defmethod matches ((psf list) (play list))
   (cond
      ((null psf)
         t
      )
      ((eq (car psf) (car play))
         (matches (cdr psf) (cdr play))
      )
      (t
         nil
      )
   )
)

(defmethod apply-rule ((rule list) &aux the-play)
   (setf the-play (fourth (fourth rule)))
   (nth (length *play-so-far*) the-play)
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

(defmethod generic-play-with-stats ((x player) (o player) (report t) &aux move)
   (setf *avail* '(nw n ne w c e sw s se))
   (setf *play-so-far* ())
   (dolist (player '(x o x o x o x o x))
      (visualize *play-so-far*)
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
   (cond
      ((eq (analyze *play-so-far*) 'w)
         (cond
            ((eq *most-recent-hmp-move* 'random)
               (setf *nr-random-moves-wins-by-hmp* (+ 1 *nr-random-moves-wins-by-hmp*))
            )
            ((eq *most-recent-hmp-move* 'heuristic)
               (setf *nr-heristic-move-wins-by-hmp* (+ 1 *nr-heristic-move-wins-by-hmp*))
            )
         )
      )
   )
   *play-so-far*
)

(defmethod random-play ((x player) (o player) (demo t) &aux move)
   (setf *avail* '(nw n ne w c e sw s se))
   (setf *play-so-far* ())
   (dolist (player '(x o x o x o x o x))
      (cond
         ((eq player 'x)
            (setf move (make-random-move x demo))
         )
         ((eq player 'y)
            (setf move (make-random-move o demo))
         )
      )
   )
   (setf *play-so-far* (snoc move *play-so-far*))
   (if (game-over-p *play-so-far*) (return nil))
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
            (return-from line t)
         )
      )
   )
   (return-from line nil)
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

(defmethod summarize-heuristic-use ()
   (format t "random move count = ~A and heuristic move count = ~A~%"
      *nr-random-moves-by-hmp* *nr-heuristic-moves-by-hmp*
   )
   (format t "random move wins = ~A and heuristic move wins = ~A~%"
      *nr-random-moves-wins-by-hmp* *nr-heuristic-moves-wins-by-hmp*
   )
   nil
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
; a heuristic machine player and a human play one game
(defmethod demo-heuristic-human ((nr-rules integer) &aux p x o)
   (setf *nr-random-moves-by-hmp* 0)
   (setf *nr-heuristic-moves-by-hmp* 0)
   (setf *nr-random-moves-wins-by-hmp* 0)
   (setf *nr-heuristic-moves-wins-by-hmp* 0)
   (setf x (make-instance 'heuristic-machine-player :name 'hm))
   (add-rules x nr-rules)
   (display x)
   (setf o (make-instance 'human-player :name 'hu))
   (display o)
   (setf p (generic-play-with-stats x o t))
   (format t "Game Summary~%")
   (format t "Play of the game = ~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   (format t "heuristic use summary~%")
   (summarize-heuristic-use)
   nil
)
; a heuristic machine player and a random machine player play one game
(defmethod demo-heuristic-random ((nr-rules integer) &aux p x o)
   (setf *nr-random-moves-by-hmp* 0)
   (setf *nr-heuristic-moves-by-hmp* 0)
   (setf *nr-random-moves-wins-by-hmp* 0)
   (setf *nr-heuristic-moves-wins-by-hmp* 0)
   (setf x (make-instance 'heuristic-machine-player :name 'hm))
   (add-rules x nr-rules)
   (display x)
   (setf o (make-instance 'random-machine-player :name 'rm))
   (display o)
   (setf p (generic-play-with-stats x o t))
   (format t "Game Summary~%")
   (format t "Play of the game = ~A~%" p)
   (visualize p)
   (format t "~A~%" (analyze p))
   (format t "heuristic use summary~%")
   (summarize-heuristic-use)
   nil
)
