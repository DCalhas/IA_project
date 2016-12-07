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
        (listStates (list ))
        (next-state nil))
        (dolist (move moves)
            (setf next-state (nextState st move))
            (setf listStates (cons next-state listStates))
        )
        (return-from nextStates listStates))
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
        (beenlist (list st))
        (currentState st)
        (velocity (state-vel st)))

        (if (isGoalp st) (return-from compute-heuristic (state-other st)))

        (setf currentState (car expandList))
        (setf expandList (cdr expandList))
        (setf (state-vel currentState) (list 0 0)) ;unitary movements, it needs to have vel at 0
        (dolist (state (nextStates currentState))
                (setf (state-vel st) velocity)
                (setf (state-other state) (+ (state-other currentState) 1))
                (when (isGoalp state) (return-from compute-heuristic (state-other state)))
                (if (not (member state beenlist :test #'comparestatesposition)) (and (setf expandList (reverse (cons state (reverse expandList)))) (setf beenlist (cons state beenlist))))
                )

        (loop
            (setf currentState (car expandList))
            (setf expandList (cdr expandList))
            (setf (state-vel currentState) (list 0 0)) ;unitary movements, it needs to have vel at 0
            (dolist (state (nextStates currentState))
                    (setf (state-other state) (+ (state-other currentState) 1))
                    (when (isGoalp state) (return-from compute-heuristic (state-other state)))
                    (if (not (member state beenlist :test #'comparestatesposition)) (and (setf expandList (reverse (cons state (reverse expandList)))) (setf beenlist (cons state beenlist))))
                    )
            )
        )
    nil)



;;; A*

(defun a* (problem)
  (let ((expandList (list (make-node :state (problem-initial-state problem)
                    :h (funcall (problem-fn-h problem) (problem-initial-state problem))
                    :g 0
                    :f (funcall (problem-fn-h problem) (problem-initial-state problem)))))
        (currentNode nil))

      (if (funcall (problem-fn-isGoal problem) (problem-initial-state problem)) (return-from a* (list (problem-initial-state problem))))

      (loop
        (stable-sort expandList #'< :key #'node-f)
        (setf currentNode (car expandList))
        (setf expandList (cdr expandList))
        (if (funcall (problem-fn-isGoal problem) (node-state currentNode)) (return-from a* (solution currentNode)))
        (dolist (next-state (funcall (problem-fn-nextStates problem) (node-state currentNode)))
          (setf expandList (cons (make-node :parent currentNode
                                            :state next-state
                                            :h (funcall (problem-fn-h problem) next-state)
                                            :g (+ (state-cost next-state) (node-g currentNode))
                                            :f (+ (funcall (problem-fn-h problem) next-state) (+ (state-cost next-state) (node-g currentNode)))) expandList))
          )
        (when (null expandList) (return-from a* nil))
        )
      )
  nil)


(defun solution (node)
  (let ((seq-states nil))
    (loop
      (when (null node)
	(return))
      (push (node-state node) seq-states)
      (setf node (node-parent node)))
    (values seq-states)))
