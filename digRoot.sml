use "digits.sml";

fun sum (l : int list) =
    if null l
    then 0
    else hd (l) + sum (tl l)

fun additivePersistence(a : int) =
    if (a mod 10) = a
    then 0
    else 
        let val l = digits(a) in
        let val b = sum(l) in
        1 + additivePersistence(b)
        end
        end

fun digitalRoot(a : int) = 
    if (a mod 10) = a
    then a
    else 
        let val l = digits(a) in
        let val b = sum(l) in
        digitalRoot(b)
        end
        end