datatype expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

datatype formula = TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr

fun evalExpr e = 
    case e of 
        NUM e => e
    |   PLUS (e1, e2) => evalExpr(e1)+evalExpr(e2)
    |   MINUS (e1, e2) => evalExpr(e1)-evalExpr(e2)

fun eval e = 
    case e of 
        TRUE => true
    |   FALSE => false
    |   NOT e => not(eval(e))
    |   ANDALSO (e1, e2) => eval(e1) andalso eval(e2)
    |   ORELSE (e1, e2) => eval(e1) orelse eval(e2)
    |   IMPLY (e1, e2) => not(eval(e1)) orelse eval(e2)
    |   LESS (e1, e2) => evalExpr(e1) < evalExpr(e2)

type name = string

datatype metro = STATION of name
               | AREA of name * metro
               | CONNECT of metro * metro

fun inList (v, l) = 
    case l of
        [] => false
    |   lh::lt => v = lh orelse inList(v, lt)

fun checkMetro m = 
    let fun cmAux(m, l) = 
        case m of
            STATION n => inList(n, l)
        |   AREA (n, inM) => cmAux(inM, n::l)
        |   CONNECT (m1, m2) => cmAux(m1, l) andalso cmAux(m2, l)
    in
        cmAux(m, [])
    end

datatype 'a lazyList = nullList
        | cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) = 
        if first = last
        then cons(first, fn () => nullList)
        else  cons(first, fn () => seq(first + 1, last))

fun infSeq(first) = cons(first, fn () => infSeq(first + 1))

fun firstN(nullList, _) = [] 
    |   firstN(_, 0) = []
    |   firstN(cons(x,f), n) = x::firstN(f(), n-1)

fun Nth(nullList, _) = NONE
    |   Nth(cons(x,_), 1) = SOME x
    |   Nth(cons(_,f),n) = Nth(f(), n - 1)

fun filterMultiples(nullList, _) = nullList
    |   filterMultiples(cons(x, f), n) = 
        if x mod n = 0
        then filterMultiples(f(), n)
        else cons(x, fn () => filterMultiples(f(), n))

fun sieve(cons(x, f)) = cons(x, fn () => sieve(filterMultiples(f(), x))) 