open Tsdl

let centered (xp, yp) t =
    let open Sdl.Rect in
    let x_mid = x t - ( w t / 2 )
    and y_mid = y t - ( h t / 2 )
    and w = w t
    and h = h t
    in
    create
    ~x:x_mid
    ~y:y_mid
    ~w
    ~h
