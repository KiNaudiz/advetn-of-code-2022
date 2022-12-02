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

(defun read-input (filename)
  (mapcar
   (lambda (pair) (mapcar #'subst-syms pair))
   (mapcar #'line-as-list (read-all-lines filename))))

(defun score-choice (choice)
  (case choice
    (rock 1)
    (paper 2)
    (scissors 3)))

(defun outcome (me opponent)
  (case me
    (rock (case opponent
             (paper 'loss)
             (rock 'tie)
             (scissors 'victory)))
    (paper (case opponent
              (scissors 'loss)
              (paper 'tie)
              (rock 'victory)))
    (scissors (case opponent
                 (rock 'loss)
                 (scissors 'tie)
                 (paper 'victory)))))

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

