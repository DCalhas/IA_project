
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track)
  (setf pos0 (nth 0 pos))
  (setf pos1 (nth 1 pos))
  (setq l (nth pos0 (track-env track)))
  (setf value (nth pos1 l))
  (if (equal nil value) t nil))


(defun isGoalp (st)
  t)

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (make-STATE :POS '(3 16)
        :VEL '(1 3)
        :ACTION act
        :COST -100))
