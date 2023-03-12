fun reverse(l: int list) = 
    if null l
    then nil
    else reverse(tl l) @ [hd l] 