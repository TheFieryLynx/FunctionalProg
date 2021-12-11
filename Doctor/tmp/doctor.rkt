#lang scheme/base
; "Доктор". Осень 2021
; В учебных целях используется базовая версия Scheme

(require racket/vector)
(require scheme/string)
(require racket/format)
(require "text.rkt")



(read_from_hash-start)
(read_from_hash-words-start)
(read_from_hash-words-end)

; подключаем функции для работы с векторами
 
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента

(define (visit-doctor stop-word patients-number)
  (let loop ((patients-count patients-number))
    (if (= patients-count 0)
        (printf "time to go home")
        (let ((patient-name (ask-patient-name)))
          (cond ((equal? patient-name (~a stop-word)) (printf "time to go home"))
                (else (printf "Hello, ~a!\n" patient-name)
                      (printf "what seems to be the trouble?")
                      (doctor-driver-loop patient-name)
                      (loop (- patients-count 1))
                )
          )
        )
    )
  )
)

; 2-5
(define (ask-patient-name)
 (begin
  (printf "next!\n")
  (printf "who are you?\n")
  (print '**)
  (car (filter non-empty-string? (string-split (read-line) #px"\\s*\\b\\s*")))
 ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента

(define (doctor-driver-loop name)
  (let loop ((rep-history #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read-response (read-line))))
      (cond 
	    ((equal? (car user-response) '("goodbye")) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week")
             (newline)
            )
            (else (display (merge-text (reply reply-strategies user-response rep-history))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (loop (vector-append (list->vector user-response) rep-history))
            )
       )
    )
  )
)

(define r_word  #px"\\s*\\b\\s*")
(define r_sent  #px"\\.|\\?|!")
(define punс_marks (list "." "," ";" ":" "-" "?" "!"))

(define (read-response str)
   (map (lambda (x)
           (filter non-empty-string? (string-split x r_word))
        )
        (string-split str r_sent)
   )
)

(define (merge-text lst)
  (let loop ((tmp_lst lst) (res ""))
    (if (null? tmp_lst)
        res
        (loop (cdr tmp_lst) (string-append (if (or
                                                   (eq? res "")
                                                   (member (car tmp_lst) punс_marks)
                                               )
                                               res
                                               (string-append res " ")
                                           ) (car tmp_lst)
                            )
        )
    )
  )
)

(define reply-strategies
  (list (list 2  (lambda(x y) #t)                                 (lambda(x y)(hedge)))
        (list 5  (lambda(x y) #t)                                 (lambda(x y)(qualifier-answer x)))
        (list 9  (lambda(x y) (if (vector-empty? y) #f #t))       (lambda(x y)(history-answer y)))
        (list 14 (lambda(x y) (if (check-for-keywords? x) #t #f)) (lambda(x y)(keywords-answer x)))
        (list 20 (lambda(x y) #t)                                 (lambda(x y)(forward-answer)))
        (list 40 (lambda(x y) #t)                                 (lambda(x y)(mix-answer x)))
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply reply-strategies user-response rep-history)
  (let ((strategy-list (filter (lambda (x)((cadr x) user-response rep-history)) reply-strategies)))
    (let ((weight (foldl (lambda(x y)(+ (car x) y)) 0 strategy-list)))
      (let loop ((rand (random weight)) (p (caar strategy-list)) (current-strategy (car strategy-list)) (other-strategies (cdr strategy-list)))
        (if (<= rand p)
            ((caddr current-strategy) user-response rep-history)
            (loop (- rand p) (caar other-strategies) (car other-strategies) (cdr other-strategies))
        )
      )
    )
  )
)

;Весна
(define (forward-answer)
   (make-answer-forward (find_random dict-cnt-hash-start))
)

(define (mix-answer user-response)
   (make-answer-mix user-response)
)

(define (history-answer rep-history)
  (cons "earlier you said that" (change-person (pick-random-vector rep-history)))
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#(("you seem to think that")
                                       ("you feel that")
                                       ("why do you believe that")
                                       ("why do you say that")
                                       ;1-1
                                       ("how often do  you feel that")
                                       ("what makes you think that")
                                       ("it appears that")
                                       ("what is the cause")
                                      )
                )
                (change-person (list-ref user-response (random (length user-response))))
        )
)

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random(vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '(("am" "are")
                           ("are" "am")
                           ("i" "you")
                           ("me" "you")
                           ("mine" "yours")
                           ("my" "your")
                           ("myself" "yourself")
                           ("you" "i")
                           ("your" "my")
                           ("yours" "mine")
                           ("yourself" "myself")
                           ("we" "you")
                           ("us" "you")
                           ("our" "your")
                           ("ours" "yours")
                           ("ourselves" "yourselves")
                           ("yourselves" "ourselves")
                           ("shall" "will"))
                         phrase)
)

;1-3
(define (many-replace replacement-pairs lst)
  (map (lambda (x)(let ((pat-rep (assoc x replacement-pairs)))
                    (if pat-rep
                        (cadr pat-rep)
                        x
                    )
                  )
       ) lst
  )
)
 
; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random-vector '#(("please go on")
                              ("many people have the same sorts of feelings")
                              ("many of my patients have told me the same thing")
                              ("please continue")
                              ;1-1
                              ("go ahead")
                              ("do not let me interrupt you")
                              ("it is ok")
                             )
        )
)

; 2-6 Структура данных, хранящая группы ключевых слов и привязанных к ним шаблонов для составления ответных реплик
(define keywords_structure '#(
     ( ; начало данных 1й группы
       ("depressed" "suicide" "exams" "university") ; список ключевых слов 1й группы
       (                                    ; список шаблонов для составления ответных реплик 1й группы 
	  ("when you feel depressed, go out for ice cream")
          ("depression is a disease that can be treated")
          ("do you like studying at the university?")
          ("how do you tolerate exams emotionally?")
       )
     ) ; завершение данных 1й группы
     ( ; начало данных 2й группы ...
       ("mother" "father" "parents" "brother" "sister" "uncle" "aunt" "grandma" "grandpa")
       (
	  ("tell me more about your "*" , i want to know all about your" *)
          ("why do you feel that way about your" * "?")
          ("do you love your "*" ?")
          ("how close are you to your "*" ?")
       )
     )
     (
       ("university" "scheme" "lections" "study" "seminars" "seminar" "lection")
       (
	  ("your education is important")
	  ("how much time do you spend on your studies ?")
          ("do you like "*" ?")
          ("do you have problems with "*" ?")
       )
     )
     (
       ("cat" "cats" "dog" "dogs" "animals" "parrot")
       (
          ("do you like your "*" ?")
          ("how often do you go for a walk with your "*" ?")
       )
     )
     (
       ("meal" "food" "breakfast" "dinner" "supper" "lunch")
       (
          ("do you eat regularly?")
          ("what is your normal portion for "*" ?")
       )
     )
  )
)

(define keywords_structure_list (vector->list keywords_structure))
(define all-keywords (foldl (lambda (x y)(foldl (lambda(z w)(if(memq z w) w (cons z w))) y (car x))) '() keywords_structure_list)); y - конечный список, (car x) - список ключевых слов группы

; 2-6 проверка наличия ключевых слов в фразе пациента
(define (check-for-keywords? user-response)
    (ormap (lambda (x) (memq x all-keywords)) user-response)
)

; 2-6 конфигурация ответа доктора если есть ключевые слова
(define (get-doctor-responses rand-word)
   (foldl (lambda (x y) (append (if (memq rand-word (car x))
                                           (cadr x)
                                           '()
                                ) y
                        )
           ) '() keywords_structure_list
   )
)

; 2-6
(define (pick-random-lst my-lst)
  (list-ref 
    my-lst 
    (random (length my-lst ))
  )
)

; 2-6 запуск стратегии с ключевыми словами
(define (keywords-answer user-response)
  (let ((filtered-user-response (foldl (lambda(x y)(if(memq x all-keywords) (cons x y) y)) '() user-response)))
    (let ((rand-word (list-ref filtered-user-response (random (length filtered-user-response)))))
      (let ((phrases-list (get-doctor-responses rand-word)))
        (many-replace (list(list `* rand-word)) (pick-random-lst phrases-list))
      )
    )
  )
)

;i am very tired. i do not want to do anything. please help me


(visit-doctor 'q 5)




