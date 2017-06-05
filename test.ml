open React
open Tsdl
open Tsdl_image
open Rresult.R.Infix

let width = 256
let height = 144
let scale = 3

let unscale (mouse_x, mouse_y) =
    (mouse_x / scale, mouse_y / scale)

let unify fn = function
    | Ok v -> v
    | Error e -> fn e

let (>>!) t f = Rresult.R.reword_error f t

let (>><) t f = unify f t

let (<<<) f g x = f @@ g x

module Option = struct
    let bind f = function
        | Some v -> f v
        | None as n -> n

    let some x = Some x

    let none = None

    let map f = bind ( some <<< f )

    let (>>=) t f = bind f t

    let (>>|) t f = map f t

    let default d = function
        | Some v -> v
        | None -> d
end

let log_msg ?prefix (`Msg e) =
    let prefix2 =
        prefix
        |> Option.map ( Printf.sprintf "%s: " )
        |> Option.default ""
    in
    Sdl.log "%s%s" prefix2 e

(*
module Errs = struct
    type 'a t =
        | Init of 'a
        | Window of 'a
        | Running of 'a
        [@@deriving variants]

    let errof errf ( `Msg m ) =
        errf m
end
*)

module Errs = struct
	let init (`Msg e) = `Init e

    let window (`Msg e) = `Window e

    let state (`Msg e) = `State e

    let running (`Msg e) = `Running e
end


type position =
    { x : int
    ; y : int
    }

let (>->) t m = t >>= fun () -> Lazy.force m
let (>~>) t m = t >>| fun () -> Lazy.force m

module Sprite : sig
    type t

    val create : Sdl.renderer -> string -> t Sdl.result
    val render : Sdl.renderer -> t -> position -> unit Sdl.result
end = struct
    type t =
        { tex : Sdl.texture
        ; w : int
        ; h : int
        }

    let create renderer filename =
        let create_sprite tex =
            Sdl.query_texture tex >>| fun (_, _, (w, h)) -> { tex; w; h }
        in
        Image.load_texture renderer filename >>= create_sprite

    let render renderer t pos =
        let dst_rect =
            Sdl.Rect.create
            pos.x
            pos.y
            t.w
            t.h
        in
        Sdl.render_copy
            ~dst:dst_rect 
            renderer
            t.tex
end

module Renderer : sig
    type t
    type colour = { r : int; g : int; b : int; a : int }
    type point = { x : int; y : int }

    val create : Sdl.renderer -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val rgb  : r:int -> g:int -> b:int -> colour

    val white : colour
    val black : colour

    val clear : t -> t
    val set_draw_color : colour -> t -> t
    val draw_line : point -> point -> t -> t
    val draw_rect : Sdl.rect -> t -> t
    val draw_sprite : Sprite.t -> position -> t -> t

    val iter : ('a -> t -> t) -> 'a list -> t -> t
    val present : t -> unit Sdl.result
end = struct
    type t =
        { renderer : Sdl.renderer
        ; steps : unit Sdl.result Lazy.t list
        }

    type colour = { r : int; g : int; b : int; a : int }

    type point = { x : int; y : int }

    let create renderer =
        { renderer; steps = [] }

    let rgba ~r ~g ~b ~a =
        { r; g; b; a }

    let rgb ~r ~g ~b =
        rgba ~r ~g ~b ~a:0xFF

    let black =
        rgb ~r:0x00 ~g:0x00 ~b:0x00

    let white =
        rgb ~r:0xFF ~g:0xFF ~b:0xFF

    let next_step t step =
        { t with steps = step :: t.steps }

    let clear t =
        next_step t @@
        lazy ( Sdl.render_clear t.renderer )

    let set_draw_color c t =
        next_step t @@
        lazy ( Sdl.set_render_draw_color t.renderer c.r c.g c.b c.a )

    let draw_line a b t =
        next_step t @@
        lazy ( Sdl.render_draw_line t.renderer a.x a.y b.x b.y )

    let draw_rect rect t =
        next_step t @@
        lazy ( Sdl.render_draw_rect t.renderer ( Some rect ))

    let draw_sprite sprite pos t =
        next_step t @@
        lazy ( Sprite.render t.renderer sprite pos )

    let iter fn items t =
        List.fold_right (fun item t -> fn item t) items t

    let present t =
        let drawn =
            List.fold_right begin fun next sofar ->
                sofar >-> next
            end t.steps
            ( Result.Ok () )
        in
        drawn >>| fun () -> Sdl.render_present t.renderer
end

type sprite_set = Sprite.t array

let idle_sprite ss tick =
    ss.(tick / 4 mod Array.length ss)

let random_sprite ss =
    ss.(Random.int @@ Array.length ss)

type sprites =
    { character : sprite_set
    }

type spray =
    { sprite : Sprite.t
    ; placement : position
    }

type state =
    { mouse : position
    ; sprites : sprites
    ; sprays : spray list
    ; z : int
    }

let tickstate tick state = (tick, state)

(*
let render r state =
    let rect =
        Rect.centered ( state.x, state.y ) @@
        Sdl.Rect.create
            ~x:state.x
            ~y:state.y
            ~w:25
            ~h:25
    in
    let rendered =
        Sdl.set_render_draw_color r 0x00 0x00 0x00 0x00 >->
        lazy (Sdl.render_clear r) >->
        lazy (Sdl.set_render_draw_color r 0xFF 0xFF 0xFF 0xFF) >->
        lazy (Sdl.set_render_draw_color r 0xFF 0x00 0x00 0xFF) >->
        lazy (Sdl.render_draw_line r 10 15 25 35) >->
        lazy (Sdl.render_draw_rect r ( Some rect )) >~>
        lazy (Sdl.render_present r)

    in
    rendered >>< log_msg ~prefix:"render err"
*)


let render r (tick, state) =
    let {mouse;sprites;sprays} = state
    in
    let black = Renderer.black
    and white = Renderer.white
    and red =
        { Renderer.black with r = 0xFF }
    and blue =
        { Renderer.black with b = 0xFF }
    and rect =
        Rect.centered ( mouse.x, mouse.y ) @@
        Sdl.Rect.create
            ~x:mouse.x
            ~y:mouse.y
            ~w:25
            ~h:25
    in

    Renderer.create r
    |> Renderer.set_draw_color black
    |> Renderer.clear
    |> Renderer.iter
        ( fun {sprite;placement} ->
            Renderer.draw_sprite sprite placement )
        sprays
    |> Renderer.set_draw_color blue
    |> Renderer.draw_line
        { Renderer.x = 10; y = 15 }
        { Renderer.x = 25; y = 35}
    |> Renderer.draw_sprite
        (idle_sprite sprites.character tick) mouse
    |> Renderer.set_draw_color { red with g = 0xff }
    |> Renderer.draw_line
        { Renderer.x = 0; y = 0 }
        { Renderer.x = mouse.x; y = mouse.y }
    |> Renderer.draw_line
        { Renderer.x = 0; y = height }
        { Renderer.x = mouse.x; y = mouse.y }
    |> Renderer.draw_line
        { Renderer.x = width; y = 0 }
        { Renderer.x = mouse.x; y = mouse.y }
    |> Renderer.draw_line
        { Renderer.x = width; y = height }
        { Renderer.x = mouse.x; y = mouse.y }
    |> Renderer.present
    >>< log_msg ~prefix:"render err"

let init renderer =
    let scale = float_of_int scale in
    let load_sprite filename =
        Sprite.create renderer filename
    in
    let character_sprites () =
        load_sprite "sprites/Player/idle/anim1.png" >>= fun idle1 ->
        load_sprite "sprites/Player/idle/anim2.png" >>= fun idle2 ->
        load_sprite "sprites/Player/idle/anim3.png" >>= fun idle3 ->
        load_sprite "sprites/Player/idle/anim4.png" >>| fun idle4 ->
        [| idle1
         ; idle2
         ; idle3
         ; idle4
        |]
    in
    Sdl.render_set_scale renderer scale scale
    >>= character_sprites
    >>| fun character ->
    { mouse =
        { x = 15
        ; y = 15
        }
    ; sprites =
        { character }
    ; sprays = []
    ; z = 0
    }

let update state = function
    | `Mouse_motion, e ->
        let x, y = unscale @@ snd @@ Sdl.get_mouse_state ()
        in
        { state with mouse = { x; y }}
    | `Mouse_button_down, e ->
        let sprite    = random_sprite state.sprites.character
        and placement = state.mouse
        in
        { state with sprays = {sprite;placement} :: state.sprays }
    | otherwise -> state

let run send ticks =
    let finished = ref false
    and t = ref 0
    and event = Sdl.Event.create ()
    and events = ref []
    in
    while not !finished do
        events := [];
        t := 1 + !t;
        while Sdl.poll_event (Some event) do
            match Sdl.Event.(enum (get event typ)) with
            | `Quit -> finished := true
            | e -> events := (e, event) :: !events
        done;
        List.iter send @@ List.rev !events;
        Sdl.delay 16l;
        ticks !t
    done

let always x y = x

let identity x = x

let main2 () =
    let bail = function
        | `Init err -> Sdl.log "error on init %s" err
        | `Window err -> Sdl.log "error on win create %s" err
        | `State err -> Sdl.log "error on state creation %s" err
        | `Running err -> Sdl.log "error while running %s" err
        ; exit 1
    and game =
        Sdl.init Sdl.Init.video
        >>! Errs.init >>= fun () ->

        Sdl.create_window_and_renderer
            ~w:(width * scale)
            ~h:(height * scale)
            Sdl.Window.shown
        >>! Errs.window >>= fun (w, renderer) ->

        init renderer
        >>! Errs.state >>= fun initial ->

        let e, send  = React.E.create () in
        let ticks, s = React.E.create () in
        let state    = React.S.fold update initial e in
        let s_fps    = React.S.sample tickstate ticks state in
        let view     = React.E.map (render renderer) s_fps
        in
        run send s;
        React.E.stop e;
        React.S.stop state;
        React.E.stop view;

        Sdl.destroy_renderer renderer;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0
        >>! Errs.running
    in

    game >>< bail


let () =
    main2 ()
