
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun isObstaclep (pos track)
  (if (equal nil (nth (nth 1 pos) (nth (nth 0 pos) (track-env track)))) t nil))


(defun isGoalp (carState)
  (let  ((endPositions (track-endpositions (state-track carState)))
        (pos (state-pos carState)))

        (dolist (iterator endPositions)
            (if (equal iterator pos)
                (return-from isGoalp t)
            )
        )
    )
    nil)

(defun nextState (st act)
  (let ((flagObs nil) (position nil) (newState nil) (posx nil) (posy nil))

  (setq position (let ((pos (state-pos st)) (velOld (state-vel st)))
                      (setq posx (first pos))
                      (setq posx (+ (first pos) (first velOld) (first act)))
                      (setq posy (second pos))
                      (setq posy (+ (second pos) (second velOld) (second act)))
                      (list posx posy)))

  (if (isObstaclep position (state-track st)) (setq flagObs t))

  (setf newState  (make-STATE :POS  (cond (flagObs (state-pos st))
                                          (t position)
                                    )
                :VEL (let ((velOld (state-vel st)) (action act))
                          (cond (flagObs '(0 0))
                                (t
                                  (setf (nth 0 velOld) (+ (nth 0 velOld) (nth 0 action)))
                                  (setf (nth 1 velOld) (+ (nth 1 velOld) (nth 1 action)))
                                  velOld)))
                :ACTION act
                :COST 0
                :TRACK (state-track st) ))

    (cond
        (flagObs (setf (state-cost newState) 20))
        ((isGoalp newState) (setf (state-cost newState) -100))
        (t (setf (state-cost newState) 1)))

    (return-from nextState newState)
    ))
