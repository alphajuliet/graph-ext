#lang racket/base
; graph-ext/main.rkt
; Extensions to the racket graph package
; AndrewJ 2018-07-21 

(require racket/function
         racket/set
         data/queue
         graph)

(provide (all-defined-out))

;------------------------
; Create a subgraph from a subset of vertices in a graph
; subgraph :: Graph -> [Vertex] -> Graph

(define (subgraph G vertices)
  (define subg (graph-copy G))
  (define removals (remove* vertices (get-vertices subg)))
  (map (curry remove-vertex! subg) removals)
  subg)

;------------------------
; Return a list of the unique vertices up to max hops from src
; get-nearest :: Graph -> Vertex (-> Integer) -> [Vertex]

(define (get-nearest G source
                     #:max-dist (max 2))
  (define acc (mutable-set source))

  ; Recursive search
  (define (add-nearest G src acc count)
    (cond ((> count 0)
           (for ([vertex (get-neighbors G src)])
             (set-add! acc vertex)
             (add-nearest G vertex acc (sub1 count)))))
    acc)

  (set->list (add-nearest G source acc max)))

;------------------------
; Define _all_ paths between two vertices.
; BEWARE, this can blow all your memory on a large graph

(define (all-paths G src dest)
  
  (define visited (mutable-set))
  (define path (make-queue))
  (define results (make-queue))

  ; Recursive function
  (define (all-paths-fn G start end visited path results)
    (set-add! visited start)
    (enqueue-front! path start)
    (if (eq? start end)
        (enqueue! results (queue->list path))
        ;else
        (for ([v (in-neighbors G start)]
              #:unless (set-member? visited v))
          (all-paths-fn G v end visited path results)))
    (dequeue! path)
    (set-remove! visited start))

  (all-paths-fn G src dest visited path results)
  (map reverse (queue->list results)))

;------------------------
; Apply a function over all paths from u to v in G

(define (all-path-fn f G u v)
  (for/hash ([p (all-paths G u v)])
    (values p (f G p))))

;------------------------
;------------------------
(module+ test
  (require rackunit
           rackunit/text-ui)
  ; Unit tests here
  )

;------------------------
(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

; The End