(defparameter *input-file* "input")
(defparameter *test-input-file* "test-input")

(defun read-all-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun string-to-list (s)
  (coerce s 'list))

(defun priority (c)
  (labels
      ((priority-lower (c)
         (1+ (- (char-int c) (char-int #\a))))
       (priority-upper (c)
         (+ 26
            (1+ (- (char-int c) (char-int #\A))))))
    (if (lower-case-p c)
        (priority-lower c)
        (priority-upper c))))

(defun split-half (l)
  (loop for x on l
        for y = x then (cddr y)
        when (null y)
          return (list (ldiff l x) x)))

(defun common-items (x y)
  (intersection x y))

(defun build-rucksacks (l)
  (mapcar #'split-half l))

(defun read-rucksacks (input)
  (let* ((input (read-all-lines input))
         (data (mapcar #'string-to-list input)))
    (mapcar #'split-half data)))

(defun unique (l)
  (cond ((null l) l)
        ((member (first l) (rest l))
         (unique (rest l)))
        (t (cons (first l) (unique (rest l))))))

(defun rucksack-common (rucksacks)
  (mapcan
   (lambda (r) (unique (common-items (first r) (second r))))
   rucksacks))

(defun task-03-01 (input)
  (let* ((rucksacks (read-rucksacks input))
         (common (rucksack-common rucksacks))
         (common-prios
           (mapcar #'priority common)))
    (reduce #'+ common-prios)))
