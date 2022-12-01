(defparameter *input-file* "input")

(defun read-all-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun split-at (sep list)
  (cond ((null list) '())
        ((equal (first list) sep) (cons '() (split-at sep (rest list))))
        (t (let ((r (split-at sep (rest list))))
             (cons (cons (first list) (first r)) (rest r))))))

(defun read-input-calories (filename)
  (let* ((lines (read-all-lines filename))
         (nums (mapcar (lambda (l) (parse-integer l :junk-allowed t))
                       lines)))
    (split-at 'nil nums)))

(defun sum-calories (cals)
  (apply '+ cals))

(defun max-calories (cals)
  (reduce #'max cals))

(defun find-max-cals-elf (filename)
  (max-calories
   (mapcar #'sum-calories
           (read-input-calories filename))))

(defun sort-cals (cals)
  (sort cals '>))

(defun top-n-cals (cals n)
  (let ((sorted-cals (sort-cals cals)))
    (subseq sorted-cals 0 n)))

(defun find-max-3-cals-elves (filename)
  (sum-calories
   (top-n-cals
    (mapcar #'sum-calories
            (read-input-calories filename))
    3)))
         
