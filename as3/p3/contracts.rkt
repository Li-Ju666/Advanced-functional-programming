(provide (contract-out
    (add_vertex (->i ([g any/c]
                      ; vertex to be inserted is integer and does not exist in the graph
                      [v (g) (and/c integer? (vert-not-exist g))])
                     (r (g v) any/c)))

    (add_edge (->i ([g any/c]
                    [v1 (g)
                               ; start vertex is an integer
                        (and/c integer?
                               ; and it exists in the graph
                               (lambda (v1) (has_vertex? g v1)))]
                    [v2 (g v1)
                               ; end vertex is an integer
                        (and/c integer?
                               ; and it exists in the graph
                               (lambda (v2) (has_vertex? g v2))
                               ; and this edge does not exists in the graph
                               (edge-not-exist g v1))]
                    ; the weight is integer
                    [w integer?])
                   ; post condition: the edge does exist in the new graph
                   (r (v1 v2) (lambda (r) (has_edge? r v1 v2)))))

    (out_neighbours (->i ([g any/c]
                          ; the requested vertex is an integer
                          [v (g) (and/c integer?
                                        ; and it exists in the graph
                                        (lambda (v) (has_vertex? g v)))])                        
                         ; the result is a list of vertices (integers)
                         (r (g v) (and/c (listof integer?)
                                         ; and there are edges between them and requested vertex
                                         (list-edge-exist g v)))))))

; function to check if a vertex is not in the graph
(define (vert-not-exist g)
    (lambda (v) (not (has_vertex? g v))))
; function to check if an edge is not in the graph
(define (edge-not-exist g v1)
    (lambda (v2) (not (has_edge? g v1 v2))))
; function to check if a list of vertices is neighbours of a vertex in the graph
(define (list-edge-exist g v)
    (lambda (l) (foldl (lambda (x acc) (if (has_edge? g v x) acc #f)) #t l)))

