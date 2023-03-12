use "reverse.sml";

fun revDigits ( a : int ) =
    if a = 0
    then []
    else (a mod 10) :: (revDigits((a - (a mod 10)) div 10))

fun digits ( a : int ) = 
    reverse(revDigits(a))