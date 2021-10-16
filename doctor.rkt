#lang scheme/base
; "Доктор". Осень 2021
; В учебных целях используется базовая версия Scheme

(require racket/vector)
; подключаем функции для работы с векторами
 
; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  ;(doctor-driver-loop name)
  (doctor-driver-loop-v2 name)
)

(define (visit-doctor-v2 stop-word patients-number)
  (let loop ((patients-count patients-number))
    (if (= patients-count 0)
        `(time to go home)
        (let ((patient-name (ask-patient-name)))
          (cond ((equal? patient-name stop-word) `(time to go home))
                (else (printf "Hello, ~a!\n" patient-name)
                      (print '(what seems to be the trouble?))
                      (doctor-driver-loop-v2 patient-name)
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
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
            )
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
            )
       )
    )
)

;1-4
(define (doctor-driver-loop-v2 name)
  (let loop ((rep-history #()))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline)
            )
            (else (print (reply-v4 reply-strategies user-response rep-history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (loop (vector-append (vector user-response) rep-history))
            )
       )
    )
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
      )
)

; 1-4
; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply-v2 user-response rep-history)
  (case (random (if (vector-empty? rep-history) 2 3))
    ((0) (qualifier-answer user-response)) ; 1й способ
    ((1) (hedge))  ; 2й способ
    ((2) (history-answer rep-history))
  )
)

; 2-6
; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply-v3 user-response rep-history)
  (case (random (if (vector-empty? rep-history) 1 0) (if (check-for-keywords? user-response) 4 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((1) (qualifier-answer user-response)) ; 1й способ  (всегда)
    ((2) (hedge))  ; 2й способ                          (всегда)
          
    ((0) (history-answer rep-history))     ;            (not(vector-empty? rep-history))
    ((3) (keywords-answer user-response))  ; 2-6        (check-for-keywords? user-response)
  )
)

; 3-7
(define reply-strategies
  (list (list 2  (lambda(x y) #t)                                 (lambda(x y)(hedge)))
        (list 5  (lambda(x y) #t)                                 (lambda(x y)(qualifier-answer x)))
        (list 9  (lambda(x y) (if (vector-empty? y) #f #t))       (lambda(x y)(history-answer y)))
        (list 14 (lambda(x y) (if (check-for-keywords? x) #t #f)) (lambda(x y)(keywords-answer x))) 
  )
)

; 3-7
; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply-v4 reply-strategies user-response rep-history)
  (let ((strategy-list (filter (lambda (x)((cadr x) user-response rep-history)) reply-strategies)))
    (let ((weight (foldl (lambda(x y)(+ (car x) y)) 0 strategy-list)))
      (let loop ((rand (random weight)) (p (caar strategy-list)) (current-strategy (car strategy-list)) (other-strategies (cdr strategy-list)))
        (if (<= rand p)
            ((caddr current-strategy) user-response rep-history)
            (loop rand (+ p (caar other-strategies)) (car other-strategies) (cdr other-strategies))
        )
      )
    )
  )
)

; 1-4
(define (history-answer rep-history)
  (append `(earlier you said that) (change-person (pick-random-vector rep-history)))
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       ;1-1
                                       (how often do  you feel that)
                                       (what makes you think that)
                                       (it appears that)
                                       (what is the cause)
                                      )
                )
                (change-person user-response)
        )
)

; случайный выбор одного из элементов вектора vctr
(define (pick-random-vector vctr)
  (vector-ref vctr (random(vector-length vctr)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace-v3 '((am are)
                           (are am)
                           (i you)
                           (me you)
                           (mine yours)
                           (my your)
                           (myself yourself)
                           (you i)
                           (your my)
                           (yours mine)
                           (yourself myself)
                           (we you)
                           (us you)
                           (our your)
                           (ours yours)
                           (ourselves yourselves)
                           (yourselves ourselves)
                           (shall will))
                         phrase)
)
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs)))   ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep
                                (cadr pat-rep)                             ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst)                                  ; иначе в начале ответа помещается прежнее начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst))     ; рекурсивно производятся замены в хвосте списка
                       )
                    )
              )
        )
)

;1-2
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((lst lst) (res '()))
    (if (null? lst)
        (reverse res)
        (let ((pat-rep (assoc (car lst) replacement-pairs)))
               (loop (cdr lst) (cons (if pat-rep
                                         (cadr pat-rep)
                                         (car lst)
                                      )
                                res)
               )
        )
    )
  )
)

;1-3
(define (many-replace-v3 replacement-pairs lst)
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
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              ;1-1
                              (go ahead)
                              (do not let me interrupt you)
                              (it is ok)
                             )
        )
)

; 2-6 Структура данных, хранящая группы ключевых слов и привязанных к ним шаблонов для составления ответных реплик
(define keywords_structure '#(
     ( ; начало данных 1й группы
       (depressed suicide exams university) ; список ключевых слов 1й группы
       (                                    ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (do you like studying at the university?)
          (how do you tolerate exams emotionally?)
       )
     ) ; завершение данных 1й группы
     ( ; начало данных 2й группы ...
       (mother father parents brother sister uncle aunt grandma grandpa)
       (
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
          (do you love your * ?)
          (how close are you to your * ?)
       )
     )
     (
       (university scheme lections study seminars seminar lection)
       (
	  (your education is important)
	  (how much time do you spend on your studies ?)
          (do you like * ?)
          (do you have problems with * ?)
       )
     )
     (
       (cat cats dog dogs animals parrot)
       (
          (do you like your * ?)
          (how often do you go for a walk with your * ?)
       )
     )
     (
       (meal food breakfast dinner supper lunch)
       (
          (do you eat regularly?)
          (what is your normal portion for * ?)
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

; 2-6 запуск конфигурации списка возможных ответов
(define (get-doctor-responses user-response)
  (foldl configure-doctor-responses '() user-response)
)

; 2-6 ищет слово среди ключевых слов и если находит, то формирует фразы должным образом и добавляет их в список
(define (configure-doctor-responses word doctor-responses-list)
  (append (foldl (lambda (x y) (append (if (memq word (car x))
                                           (map (lambda(z)(many-replace-v3 (list(list `* word)) z)) (cadr x))
                                           '()
                                       ) y
                               )
                 ) '() (vector->list keywords_structure)
          ) doctor-responses-list
  )
)

; 2-6 запуск стратегии с ключевыми словами
(define (keywords-answer user-response)
  (let ((phrases-list (get-doctor-responses user-response)))
      (list-ref phrases-list (random (length phrases-list)))
  )
)








