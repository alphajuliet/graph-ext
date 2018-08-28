# graph-ext

Extensions to the Racket [Generic Graph package](https://docs.racket-lang.org/graph/index.html).

## Functions

* `(subgraph G vertices)`: return a subgraph just containing the given vertices.
* `(get-nearest G v [#:max-dist 2])`: get the neighbourhood of vertices within radius `max-dist`.
* `(all-paths G src dest)`: return all paths connecting `src` and `dest`.
* `(all-path-fn f G src dest)`: apply a function (e.g. length) across all paths from `src` to
`dest`.
* `(find-maximal-cliques G)`: find all maximal cliques of `G` using the
Bron-Kerbosch algorithm.

