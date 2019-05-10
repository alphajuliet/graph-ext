#lang racket/base
; graph-ext/main.rkt
; Extensions to the racket graph package
; AndrewJ 2018-07-21 

(require racket/function
         racket/set
         racket/list
         data/queue
         graph
         xml)

(provide (all-defined-out))

; Main functions:
; - count-vertices
; - count-edges
; - undirected?
; - subgraph
; - get-nearest
; - all-paths
; - all-path-fn
; - find-maximal-cliques
; - density
; - create-gexf
; - write-gexf

;------------------------
; Check if undirected.
; Not very efficient implementation.
(define (undirected? G)
  (define edges (get-edges G))
  (equal? (sort (map first edges) symbol<?)
          (sort (map second edges) symbol<?)))

;------------------------
; Count things
(define (count-vertices G)
  (length (get-vertices G)))

(define (count-edges G)
  (length (get-edges G)))
  
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
; Find all cliques using Bron-Kerbosch
; https://towardsdatascience.com/graphs-paths-bron-kerbosch-maximal-cliques-e6cab843bc2c

(define (find-maximal-cliques G)

  ;BronKerbosch1(R, P, X):
  ;  if P and X are both empty:
  ;    report R as a maximal clique
  ;  for each vertex v in P:
  ;    BronKerbosch1(
  ;      R ⋃ {v}, 
  ;      P ⋂ N(v), 
  ;      X ⋂ N(v)
  ;    )
  ;    P := P \ {v}
  ;    X := X ⋃ {v}
  (define (bron-kerbosch G acc r p x)
    (if (and (set-empty? p)
             (set-empty? x))
        (append acc (list (set->list r)))
        ;else
        (begin
          (for* ([v (in-set p)])
            (define nv (list->set (get-neighbors G v)))
            (set! acc (bron-kerbosch G
                                     acc
                                     (set-add r v)
                                     (set-intersect p nv)
                                     (set-intersect x nv)))
            (set! p (set-remove p v))
            (set! x (set-add x v)))
          acc)))

  (define R (set))
  (define P (list->set (get-vertices G)))
  (define X (set))
  (define accum '())
  
  (bron-kerbosch G accum R P X))

;------------------------
; Calculate the graph density
; For an undirected graph, only use one edge between two vertices 
(define (density G)
  (let ([r (count-edges G)]
        [n (count-vertices G)])
    
    (if (undirected? G)
        (/ r (* n (- n 1)))
        (/ (* 2 r) (* n (- n 1))))
    ))

;------------------------
; Export graph to GEXF
; For import into visualisation apps like Gephi
; It assumes that the graph is undirected.

(define (create-gexf G)
  (define gr '(graph ((defaultedgetype "undirected"))))
  (define xnodes (for/list ([v (get-vertices G)])
                   `(node ((id ,(symbol->string v))
                           (label ,(symbol->string v))))))
  (define xedges (for/list ([e (get-edges G)])
                   `(edge ((id ,(symbol->string (gensym)))
                           (source ,(symbol->string (first e)))
                           (target ,(symbol->string (second e)))
                           (weight ,(number->string (edge-weight G (first e)
                                                                 (second e))))))))
  
  (string-append "<?xml version='1.0' encoding='utf-8'?>"
                 (xexpr->string
                  `(,@gr (nodes () ,@xnodes)
                         (edges () ,@xedges)))))

; Write GEXF to a file
(define (write-gexf G fname)
  (call-with-output-file fname #:exists 'replace
    (λ (out) (display (create-gexf G) out))))

;------------------------
; Unit tests

(module+ test
  (require rackunit
           rackunit/text-ui)

  ; Test data
  (define g0 (weighted-graph/undirected
              '((1 amy erin)
                (2 amy jack)
                (2 erin jack)
                (3 erin sally))))
  
  ; Tests
  (define graph-ext-tests
    (test-suite
     "graph-ext unit tests"
     
     (check-true (graph? g0))
     (check-equal? (count-vertices g0) 4)
     (check-true (undirected? g0))

     ; Subgraph
     (let ([g1 (subgraph g0 '(amy erin jack))])
       (check-equal? (count-vertices g1) 3))

     (test-case
      "All paths"
      (check-equal? (length (all-paths g0 'amy 'sally)) 2)
      (check-equal? (all-path-fn (λ (g p) (length p)) g0 'amy 'sally)
                    '#hash(((amy erin sally) . 3) ((amy jack erin sally) . 4))))
     
     (check-equal? (length (find-maximal-cliques g0)) 2)

     (check-equal? (density g0) 2/3)
     
     (test-case
      "GEXF export"
      (let ([x (create-gexf g0)])
        (check-true (string? x))))))
  
  (run-tests graph-ext-tests))

;------------------------
(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )

; The End