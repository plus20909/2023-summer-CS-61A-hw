(define (no-repeats lst) 
  (if (null? lst)
      '()
      (cons (car lst)
            (no-repeats
             (filter(lambda(x)(not (= x(car lst))))lst)))))

(define (student-attend-class student class)
  (let((name(student-get-name student))
        (classes(cons class(student-get-classes student))))
    (student-create name classes)))

(define (teacher-hold-class teacher)
  (define students (teacher-get-students teacher)) ; Get the list of student instances
  (define new-students (map(lambda (student)(student-attend-class student (teacher-get-class teacher))) students))
  (teacher-create (teacher-get-name teacher)
                  (teacher-get-class teacher)
                  new-students))


(define (add-leaf t x)
  (if (is-leaf t)
      t
      (begin (define mapped-branches
                     (map (lambda(branch)(add-leaf branch x)) (branches t)))
             (tree (label t)
                   (append mapped-branches (list (tree x '())))))))
