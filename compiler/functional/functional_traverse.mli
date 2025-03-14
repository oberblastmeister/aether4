type ('t, 's, 'b, 'a) t = 'a -> f:('s -> 't) -> 'b
type ('s, 'a) t' = ('s, 's, 'a, 'a) t

val compose : ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t
val ( & ) : ('t, 's, 'b, 'a) t -> ('v, 'u, 't, 's) t -> ('v, 'u, 'b, 'a) t
val filtered : f:('s -> bool) -> ('s, 's) t'
