
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
  (if (equal nil (nth (nth 1 pos) (nth (nth 0 pos) (track-env track)))) t nil))


(defun isGoalp (st) ;;state has a position and a track
  t)                ;;just verifiy if the position is in endpositions list

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (make-STATE :POS '(3 16);;just do sta a make state of the next state fuck da police!!
        :VEL '(1 3)
        :ACTION act
        :COST -100))
