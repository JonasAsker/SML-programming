fun merge(l1: int list, l2: int list) =
    if null l1
    then l2
    else if null l2
    then l1
    else if hd l1 < hd l2
    then hd l1 :: merge(tl l1, l2)
    else hd l2 :: merge(l1, tl l2)
    