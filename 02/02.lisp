(defparameter *input-file* "input")

(defun read-all-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun line-as-list (line)
  (read-from-string (concatenate 'string "(" line ")")))

(defun subst-syms (symbol)
  (case symbol
    ((a x) 'rock)
    ((b y) 'paper)
    ((c z) 'scissors)))

(defun subst-syms-2 (symbol)
  (case symbol
    (a 'rock)
    (b 'paper)
    (c 'scissors)
    (x 'loss)
    (y 'tie)
    (z 'victory)))

(defun read-input (filename)
  (mapcar
   (lambda (pair) (mapcar #'subst-syms pair))
   (mapcar #'line-as-list (read-all-lines filename))))

(defun read-input-2 (filename)
  (mapcar
   (lambda (pair) (mapcar #'subst-syms-2 pair))
   (mapcar #'line-as-list (read-all-lines filename))))

(defun score-choice (choice)
  (case choice
    (rock 1)
    (paper 2)
    (scissors 3)))

(defun circular (items)
  (setf (cdr (last items)) items)
  items)

(defparameter *game-options*
  (circular '(rock paper scissors)))

(defun outcome (me opponent)
  (cond ((eq me opponent) 'tie)
        ((eq (second (member me *game-options*)) opponent)
         'loss)
        (t 'victory)))

(defun score-win (pair)
  (case (outcome (second pair) (first pair))
    (loss 0)
    (tie 3)
    (victory 6)))

(defun score-round (pair)
  (+
   (score-choice (second pair))
   (score-win pair)))

(defun score-total (matches)
  (apply #'+ (mapcar #'score-round matches)))

(defun for-outcome (outcome opponent)
  (case outcome
    (tie opponent)
    (victory (second (member opponent *game-options*)))
    (loss (third (member opponent *game-options*)))))

(defun construct-game-list (outcome-pairs)
  (mapcar
   (lambda (l)
     (list
      (first l)
      (for-outcome (second l) (first l))))
   outcome-pairs))

(defun task-02-01 (input)
  (score-total (read-input input)))

(defun task-02-02 (input)
  (score-total (construct-game-list (read-input-2 input))))
