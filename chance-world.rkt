;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname chance-world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/base)
(require racket/list)



(define POEM-LEN 8)
(define LINE-LEN 4)
(define FADE-SPEED 10)


(define file (open-input-file "poemwords.txt"))
(define TEXT (read-line file))
(close-input-port file)

(define-struct phrase (text fade))

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
                                 (= 1 (random POEM-LEN (make-pseudo-random-generator))))
                             poem]
                            [else
                             (local [(define pick (random (length words) (make-pseudo-random-generator)))
                                     (define chosen (make-phrase (list-ref words pick) 0))]
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

(define EX1 (make-phrase "My " 40))
(define LEX2 (list (make-phrase "my " 40) (make-phrase "Name " 23 )))


(define-struct poem (lot line phr))

(define POEM (make-poem (get-poem TEXT) 0 0))

(define TEXT-SIZE 20)
(define MTS (empty-scene 900 (* TEXT-SIZE (length (poem-lot POEM)))))

;; ===== WORLD


(define (main p)
  (big-bang p
       (on-tick (add-word p))
       (to-draw (render p))
       (stop-when (stop? p))))

(define (stop? p)
  (>= (poem-line p) (length (poem-lot p))))



(define (add-word p)
  (cond [(>= (poem-phr p)
             (length (list-ref (poem-line p) (poem-lot p))))
         (make-poem (poem-lot p)
                    (add1 (poem-line p))
                    0)]
        [else (make-poem (poem-lot p)
                         (poem-line p)
                         (add1 (poem-phr p)))]))


(define (render p)
  (above
   (render-lines
   (take (poem-lot p) (poem-line p)))
  (beside (render-line (take (poem-line p) (poem-lot p)))
          (render-phr (poem-phr p) (list-ref (poem-line p) (poem-lot p))))))

(define (render-lines lol)
  (cond [(empty? lol) empty-image]
        [else (above (render-line (first lol))
                     (render-lines (rest lol)))]))

(define (render-line lop)
  (cond [(empty? lop) empty-image]
        [else (beside (render-phr (first lop))
                      (render-line (rest lop)))]))

(define (render-phr phr)
  (text (phrase-text phr) TEXT-SIZE (colour-get (phrase-fade phr))))

(define (colour-get alpha)
  (make-color 255 255 255 alpha))