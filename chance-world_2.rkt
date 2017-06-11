;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chance-world_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/base)
(require racket/list)



(define POEM-LEN 8)
(define LINE-LEN 4)
(define SPEED 10)


(define file (open-input-file "poemwords.txt"))
(define TEXT (read-line file))
(close-input-port file)


;; poem with list of phrases and the current phrase indicated by line, pos no


;; generate poem
;; calculate MTS width
;; on render, fade in current



;; ==== GENERATE THE POEM
;; String -> (listof String)
;; separates long string into words


(define (get-poem txt)
  (local [(define (get-words txt)
            (local [(define (get-words res txt)
                      (cond [(string=? "" txt) res]
                            [else
                             (local [(define new-txt (trim-space txt))
                                     (define splice (find-next-space new-txt 0))]
                               (get-words
                                (append res (list (substring new-txt 0 splice)))
                                (substring new-txt splice)))]))
                    
                    (define (find-next-space txt num)
                      (cond [(string=? "" txt) num]
                            [(string=? " " (string-ith txt 0)) num]
                            [else (find-next-space (substring txt 1) (add1 num))]))
                    
                    
                    (define (trim-space txt)
                      (if (string=? (substring txt 0 1) " ")
                          (substring txt 1)
                          txt))]
              (get-words empty txt)))
          
          
          ;; (listof String) -> (listof String)
          ;; randomly chops lists of words into concatenated phrases
          (define (parse-text txt)
            (local [(define STR-LEN (length txt))
                    (define (parse-word res txt pos)
                      (local [(define splice (random 4 (make-pseudo-random-generator)))]
                        (cond [(> (+ pos splice) STR-LEN) res]
                              [else
                               (parse-word (cons (cat-all-words (take txt splice) "") res)
                                           (drop txt splice)
                                           (+ pos splice))])))
                    
                    
                    (define (cat-all-words low str)
                      (cond [(empty? low) str]
                            [else (cat-all-words
                                   (rest low)
                                   (string-append (first low) " " str))]))]
              (parse-word empty txt 0)))
          
          (define (chop txt)
            (parse-text (get-words txt)))
          
          
          ;; (listof String) -> (listof Phrase)
          (define (write-poetry words)
            (local [(define (assemble poem line words nlcount)
                      (cond [(or (empty? words)
                                 (= (length poem) POEM-LEN))
                             poem]
                            [else
                             (local [(define pick (random (length words) (make-pseudo-random-generator)))
                                     (define chosen (list-ref words pick))]
                               (if (= 0 nlcount)
                                   (assemble (cons (cons chosen line) poem)
                                             empty
                                             (remove chosen words)
                                             (random LINE-LEN (make-pseudo-random-generator)))
                                   (assemble poem
                                             (cons chosen line)
                                             (remove chosen words)
                                             (sub1 nlcount))))]))]
              (assemble empty empty words LINE-LEN)))]
    
    
    (write-poetry (chop txt))))


(define-struct poem (lot line phr a))

(define POEM (make-poem (get-poem TEXT) 1 1 0))

(define TEXT-SIZE 12)
(define MTS (empty-scene 900 (* TEXT-SIZE (+ 3 (length (poem-lot POEM))))))

;; ===== WORLD


(define (main p)
  (big-bang p
            (on-tick add-word)
            (to-draw render)
            (stop-when stop?)))

(define (stop? p)
  (>= (add1 (poem-line p)) (length (poem-lot p))))



(define (add-word p)
  (cond [(>= (+ (poem-a p) SPEED) 255)
         (if (>= (add1 (poem-phr p))
             (length (list-ref (poem-lot p) (poem-line p))))
             (make-poem (poem-lot p)
                    (add1 (poem-line p))
                    0
                    0)
             (make-poem (poem-lot p)
                    (poem-line p)
                    (add1 (poem-phr p))
                    0))]
        [else (make-poem (poem-lot p)
                         (poem-line p)
                         (poem-phr p)
                         (+ (poem-a p) SPEED))]))


(define (render p)
  (if (empty? (poem-lot p))
      (text "LMAO" 12 "black")
      (overlay
       (above
       (render-lines (take (poem-lot p) (sub1 (poem-line p))))
       (beside (render-line (list-ref (poem-lot p) (sub1 (poem-line p)))) ;; remove sub1 ;; displaying each word multiple times 
               (render-phr (list-ref (list-ref (poem-lot p) (poem-line p))(poem-phr p)) (poem-a p))))
               MTS)))

(define (render-lines lol)
  (cond [(empty? lol) empty-image]
        [else (above (render-line (first lol))
                     (render-lines (rest lol)))]))

(define (render-line lop)
  (cond [(empty? lop) empty-image]
        [else (beside (render-phr (first lop) 255)
                      (render-line (rest lop)))]))

(define (render-phr phr alpha)
  (text phr TEXT-SIZE (colour-get alpha)))


(define (colour-get alpha)
  (make-color 255 0 255 alpha))