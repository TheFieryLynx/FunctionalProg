#lang scheme/base

(require scheme/string)
(require racket/serialize)
(require racket/set)
(provide (all-defined-out))

(define input_data "data.txt")
(define dict-cnt-hash-start_data "dict-cnt-hash-start.txt")
(define dict-hash-words-start_data "dict-hash-words-start.txt")
(define dict-hash-words-end_data "dict-hash-words-end.txt")

;чтение 
(define (read_from_hash-start)
  (define in (open-input-file dict-cnt-hash-start_data))
  (define tmp_read (make-hash))
  (set! tmp_read (read in))
  (map (lambda(x)(hash-set! dict-cnt-hash-start x (hash-ref tmp_read x))) (hash-keys tmp_read))
  (close-input-port in)
)

(define (read_from_hash-words-start)
  (define in (open-input-file dict-hash-words-start_data))
  (define tmp_read (make-hash))
  (set! tmp_read (read in))
  (map (lambda(x)(hash-set! dict-hash-words-start x (hash-ref tmp_read x))) (hash-keys tmp_read))
  (close-input-port in)
)

(define (read_from_hash-words-end)
  (define in (open-input-file dict-hash-words-end_data))
  (define tmp_read (make-hash))
  (set! tmp_read (read in))
  (map (lambda(x)(hash-set! dict-hash-words-end x (hash-ref tmp_read x))) (hash-keys tmp_read))
  (close-input-port in)
)

;запись
(define (write_to_file ht_table file)
  (define out (open-output-file file	#:exists 'replace))
  (write ht_table out) 
  (close-output-port out)
)


;N
(define N 3)

;таблица N-1 грамм c числом встреч начал в датасете
(define dict-cnt-hash-start (make-hash))

;таблица с последующими словами и их количеством
(define dict-hash-words-start (make-hash))

;таблица с предыдущими словами и их количеством
(define dict-hash-words-end (make-hash))

(define r_word  #px"\\s*\\b\\s*")
(define r_sent  #px"\\.|\\?|!")
(define punс_marks (list "." "," ";" ":" "-" "?" "!"))
(define end_sent (list "." "?" "!"))

(define (parse_string str)
   (map (lambda (x)
           (reverse (cons "." (reverse (filter non-empty-string? (string-split x r_word)))))
        )
        (string-split (string-downcase str) r_sent)
   )
)

(define (text_in_dict filename)
    (let ((input (open-input-file filename)))
        (let loop ((line (read-line input)))
           (if (eof-object? line)
               (printf "\n\nEverything is ready\n\n")
               (let ((parsed (parse_string line)))
                   (map sentece-to-Ngrams parsed)
                   (loop (read-line input))
               )
           )
        )
    )
)

(define (sentece-to-Ngrams sent)
   (if (>= (length sent) N) ;если длина предложения не меньше N
       (begin
         (let LOOP-1 ((word-num 0) (res-lst '()))
           (if (< word-num (- (length sent) (sub1 N)))
               (let N-loop ((cnt 0)(n-gram '()))
               
                 (if (= cnt (sub1 N))
                     (if (= word-num 0)
                         (begin 
                           (add-to-dict-cnt-start n-gram)
                           (add-to-dict-hash-words-start n-gram (list-ref sent (+ word-num cnt))) 
                           (LOOP-1 (add1 word-num) (append res-lst  n-gram))
                           )
                         (begin
                           (add-to-dict-hash-words-start  n-gram (list-ref sent (+ word-num cnt))) 
                           (LOOP-1 (add1 word-num) (append res-lst  n-gram))
                           )
                         )
                     (N-loop (add1 cnt)(append n-gram (list (list-ref sent (+ word-num cnt)))))
                     )
                 )
               res-lst
               )
         )
         (let LOOP-2 ((word-num (- (length sent) (sub1 N))) (res-lst '()))
           (if (> word-num 0)
               (let N-loop ((cnt 0)(n-gram '()))
                 (if (= cnt (sub1 N))
                   
                     (begin 
                       (add-to-dict-hash-words-end n-gram (list-ref sent (sub1 word-num))) 
                       (LOOP-2 (sub1 word-num) (append res-lst n-gram))
                     )
                         
                     (N-loop (add1 cnt)(append n-gram (list (list-ref sent (+ word-num cnt)))))
                 )
                 )
               res-lst
               )
           )
       )
       '()
   )
)


(define (add-to-dict-cnt-start n-gram)
    (if (hash-has-key? dict-cnt-hash-start n-gram)
        (hash-set! dict-cnt-hash-start n-gram (add1 (hash-ref dict-cnt-hash-start n-gram)))
        (hash-set! dict-cnt-hash-start n-gram 1)
    )
)

; 1) "прямой" способ
(define (add-to-dict-hash-words-start n-gram word)
    (if (hash-has-key? dict-hash-words-start n-gram)
        (let ((words-hash (hash-ref dict-hash-words-start n-gram)))
             (if (hash-has-key? words-hash word)
                  (hash-set! words-hash word (add1 (hash-ref words-hash word)))
                  (hash-set! words-hash word 1)
             )
        )
        (hash-set! dict-hash-words-start n-gram (make-hash (list (cons word 1))))
    )
)

; 2) "обратный"
(define (add-to-dict-hash-words-end n-gram word)
    (if (hash-has-key? dict-hash-words-end n-gram)
        (let ((words-hash (hash-ref dict-hash-words-end n-gram)))
             (if (hash-has-key? words-hash word)
                  (hash-set! words-hash word (add1 (hash-ref words-hash word)))
                  (hash-set! words-hash word 1)
             )
        )
        (hash-set! dict-hash-words-end n-gram (make-hash (list (cons word 1))))
    )
)

(define (find_random hash_table)
    (let loop ((list_keys (hash-keys hash_table)) (rand (random 1 (foldl + 1 (hash-values hash_table))) )) 
      (if (<= rand (hash-ref hash_table (car list_keys)))
          (car list_keys)
          (loop (cdr list_keys) (- rand (hash-ref hash_table (car list_keys))))
      )
    )
)

; прямой способ построения реплик
(define (make-answer-forward start-n-1-gram)
    (let loop ((n-1-gram start-n-1-gram) (res_phrase (reverse start-n-1-gram)))
      (let ((next (find_random (hash-ref dict-hash-words-start n-1-gram)))) 
        (if (set-member? end_sent next)
            (reverse (cons next res_phrase))
            (loop (append (cdr n-1-gram) (list next) ) (cons next res_phrase))
        )
      )
   )
)

; обратный способ построения реплик для смешанного
(define (make-answer-back first)
  (let loop ((n-1-gram first) (res_phrase '()))
    (let ((next (find_random (hash-ref dict-hash-words-end n-1-gram))))
      (let ((next-n-1-gram (cons next (reverse (cdr (reverse n-1-gram))))))
        (if (hash-ref dict-cnt-hash-start next-n-1-gram #f)
            (cons next res_phrase)
            (loop next-n-1-gram (cons next res_phrase))
       )
     )
   )    
 )
)

(define (to-n-grams sent)
   (define sent-n-1-grams (make-hash))
   (if (>= (length sent) N) ;если длина предложения не меньше N
        (let LOOP-1 ((word-num 0) (res-lst '()))
           (if (<= word-num (- (length sent) (sub1 N)))
               (let N-loop ((cnt 0)(n-gram '()))
               
                 (if (= cnt (sub1 N))
                     (begin
                       (if (hash-has-key? sent-n-1-grams n-gram)
                           (hash-set! sent-n-1-grams n-gram (add1 (hash-ref dict-cnt-hash-start n-gram)))
                           (hash-set! sent-n-1-grams n-gram 1)
                       )
                       (LOOP-1 (add1 word-num) (append res-lst  n-gram))
                     )
                  
                     (N-loop (add1 cnt)(append n-gram (list (list-ref sent (+ word-num cnt)))))
                 )
               )
               sent-n-1-grams
            )
        ) 
        '()
   )
)

(define (check-in-global-hash user-response)
  (let LOOP-SENT ((sent-num 0))
       (if (< sent-num (length user-response))
           (let ((n-1-grams (to-n-grams (list-ref user-response sent-num))))
              (let ((keys (hash-keys n-1-grams)))
                 (let loop-keys ((i 0))
                    (if (< i (length keys))
                        (if (hash-has-key? dict-hash-words-start (list-ref keys i))
                            (list-ref keys i)
                            (LOOP-SENT (add1 sent-num))
                        )
                        (loop-keys (add1 i))
                    )
                 )
                
              )
           )
           #f
       )
   )
)

(define (make-answer-mix user-response)
   (let ((check (check-in-global-hash user-response)))
     (if check
         (append (make-answer-back check) (make-answer-forward check))
         (make-answer-forward (find_random dict-cnt-hash-start))
     )
   )
   
  
   ;(append (make-answer-back part_user_response) (make-answer-forward part_user_response) )
)
;this is one of the most interesting questions

;(("i" "am" "very1" "very2" "very3" "tired") ("i" "is" "one" "of" "this" "shit") ("kek" "fdsf" "fdsgdfg" "gf" "gdfgdf"))
;(("i" "am" "very1" "very2" "very3" "tired") ("fdsf" "fdsffd" "one" "of" "this" "shit") ("kek" "fdsf" "fdsgdfg" "gf" "gdfgdf"))
;(make-answer-mix '(("i" "am" "very1" "very2" "very3" "tired") ("i" "is" "one" "of" "this" "shit") ("kek" "fdsf" "fdsgdfg" "gf" "gdfgdf")))

(if #f
    (begin
      ;(read_from_hash-start)
      ;(read_from_hash-words-start)
      ;(read_from_hash-words-end)

      ;(map (lambda(x)(hash-set! my-try x (hash-ref dict-hash-words-start x))) (hash-keys dict-cnt-hash-start))

      (text_in_dict input_data)

      (write_to_file dict-cnt-hash-start dict-cnt-hash-start_data)
      (write_to_file dict-hash-words-start dict-hash-words-start_data)
      (write_to_file dict-hash-words-end dict-hash-words-end_data)
    )
    "kek"
)