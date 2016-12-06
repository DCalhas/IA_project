(load "datastructures.lisp")
(load "auxfuncs.lisp")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st)
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir
(defun nextStates (st)
  (let ((moves (possible-actions))
        (listStates (list )))
        (dolist (move moves)
            (setq listStates (cons (nextState st move) listStates))
        )
        listStates)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VER PSEUDOCODIGO PARA VERIFCAR ERRRO
;;; limdepthfirstsearch lim = 6
(defun limdepthfirstsearch (problem lim)
  (let ((state (problem-initial-state problem)))
    (if (funcall (problem-fn-isGoal problem) state) (return-from limdepthfirstsearch (list state));;T
      (if (not (equal lim 0))
        (let ((states (funcall (problem-fn-nextStates problem) state))
              (solution nil))

            (dolist (nextNode states)
              (setq solution (limdepthfirstsearch (make-problem  :initial-state nextNode
                                                  :fn-isGoal (problem-fn-isGoal problem)
                                                  :fn-nextstates (problem-fn-nextStates problem))
                                                  (- lim 1)))
              (if (and (not (equal solution ':corte)) solution) (return-from limdepthfirstsearch (cons state solution)))
            )
            nil
        )
        (return-from limdepthfirstsearch ':corte)
      )
    )
    nil)
)


(defun iterlimdepthfirstsearch (problem)
  (let ((sol nil)
        (lim 0))
    (loop
      (setq sol (limdepthfirstsearch problem lim))
      (if (or (equal sol ':corte) (equal sol nil)) t (return-from iterlimdepthfirstsearch lim))
      (setq lim (+ lim 1))
    )
  )
  nil
)
;; Solution of phase 3

;; Heuristic
(defun comparestatesposition (state1 state2)
  (return-from comparestatesposition (equal (state-pos state1) (state-pos state2))))

(defun compute-heuristic (st)
  (setf (state-other st) 0)
	(let ((expandList (list st))
        (beenlist (list ))
        (nextStatesList (nextStates st))
        (currentState nil))
        (return-from compute-heuristic 0)
        (if (isGoalp st) (return-from compute-heuristic (state-other st)))

        (setf currentState (car expandList))
        (setf expandList (cdr expandList))
        (setf beenlist (cons currentState beenlist))


        (loop
            (dolist (state nextStatesList)
                    (setf (state-other state) (+ (state-other currentState) 1))
                    (if (isGoalp state) (write "cnsocisin"))
                    (if (not (member state beenlist :test #'comparestatesposition)) (and (setf expandList (reverse (cons state (reverse expandList)))) (setf beenlist (cons state beenlist))))
                    )
            (setf currentState (car expandList))
            (setf expandList (cdr expandList))
            (setf nextStatesList (nextStates currentState))
            (when (equal expandList '()) (return-from compute-heuristic ':nasoci))
            )
        )
    nil)



;;; A*
(defun a* (problem)
  (list (make-node :state (problem-initial-state problem))))