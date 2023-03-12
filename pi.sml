fun pi (a: int, b: int, f: int -> int) =
    if a = b
    then f a
    else f (a) * pi(a + 1, b, f)