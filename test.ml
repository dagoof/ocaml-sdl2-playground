open React
open Tsdl
open Tsdl_image
open Rresult.R.Infix

let width = 256
let height = 144
let scale = 3

let unscale (mouse_x, mouse_y) =
    (mouse_x / scale, mouse_y / scale)

let key_scancode e =
    Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) 

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

module Point = struct
    type 'a t =
        { x : 'a
        ; y : 'a
        }

    let map fn t =
        { x = fn t.x
        ; y = fn t.y
        }

    let fangle t1 t2 =
        atan2 (t2.y -. t1.y) (t2.x -. t1.x)

    let angle p1 p2 =
        let t1 = map float_of_int p1
        and t2 = map float_of_int p2
        in
        fangle t1 t2

    let fdistance t1 t2 =
        let dx = t2.x -. t1.x
        and dy = t2.y -. t1.y
        in
        sqrt @@ (dx ** 2.0) +. (dy ** 2.0)

    let distance p1 p2 =
        let f1 = map float_of_int p1
        and f2 = map float_of_int p2
        in
        fdistance f1 f2

    let x t = t.x

    let y t = t.y

    let of_position pos = { x = pos.x; y = pos.y }

    let create ~x ~y = { x; y }

    let rect ~w ~h t =
        Sdl.Rect.create ~x:t.x ~y:t.y ~w ~h
end

let center =
    Point.create
        ~x:(width / 2)
        ~y:(height / 2)

type position = int Point.t

let (>->) t m = t >>= fun () -> Lazy.force m
let (>~>) t m = t >>| fun () -> Lazy.force m

module Sprite : sig
    type t
    type direction = [ `Horizontal | `Vertical | `None ]

    val w : t -> int
    val h : t -> int

    val create : Sdl.renderer -> string -> t Sdl.result
    val render : Sdl.renderer -> t -> position -> unit Sdl.result
    val flip : direction -> t -> t
end = struct
    type direction =
        [ `Horizontal | `Vertical | `None ]

    type t =
        { tex : Sdl.texture
        ; w : int
        ; h : int
        ; flip : direction
        }

    let w t = t.w

    let h t = t.h

    let create renderer filename =
        let create_sprite tex =
            Sdl.query_texture tex >>| fun (_, _, (w, h)) ->
                { tex; w; h; flip = `None}
        in
        Image.load_texture renderer filename >>= create_sprite

    let sdl_flip = function
        | `Horizontal -> Sdl.Flip.horizontal
        | `Vertical   -> Sdl.Flip.vertical
        | `None       -> Sdl.Flip.none

    let flip direction t =
        { t with flip = direction }

    let render renderer t pos =
        let dst_rect =
            Sdl.Rect.create
            pos.Point.x
            pos.Point.y
            t.w
            t.h
        in
        match t.flip with
        | `None ->
            Sdl.render_copy
                ~dst:dst_rect 
                renderer
                t.tex
        | otherwise ->
            Sdl.render_copy_ex
            ~dst:dst_rect
            renderer
            t.tex
            0.0
            None
            (sdl_flip otherwise)
end

type sprite_set = Sprite.t array

let pi = 4.0 *. atan 1.0

let pi2 = pi *. 2.0  

module Vector : sig
    type t =
        { t : float
        ; v : float
        }

    val create : v:float -> t:float -> t
    val t : t -> float
    val v : t -> float

    val dx_dy : t -> float * float
    val add : t -> t -> t
    val point : t -> float Point.t

    val scale : float -> t -> t
    val adjust_point : float Point.t -> t -> float Point.t
    val adjust_ipoint : int Point.t -> t -> int Point.t
end = struct
    type t =
        { t : float
        ; v : float
        }

    let create ~v ~t = {v; t}

    let t _t = _t.t

    let v _t = _t.v

    let dx_dy {t;v} =
        let x = ( cos t ) *. v
        and y = ( sin t ) *. v
        in
        (x, y)

    let point t =
        let x, y = dx_dy t in
        Point.create ~x ~y

    (*
    let add a b =
        let pa = point a
        and pb = point b
        in
        { v = Point.fdistance pa pb
        ; t = Point.fangle pa pb
        }
    *)

    let scale x t =
        { t with v = t.v *. x }

    let adjust_point p t =
        let dx, dy = dx_dy t in
        let x = Point.x p +. dx
        and y = Point.y p +. dy
        in
        Point.create ~x ~y

    let adjust_points vs =
        List.fold_right begin fun v p ->
            adjust_point p v
        end
        vs
        (Point.create ~x:0.0 ~y:0.0)


    let add a b =
        let origin = Point.create ~x:0.0 ~y:0.0 in
        let first  = adjust_point origin a in
        let moved  = adjust_point first b
        in
        { v = Point.fdistance origin moved
        ; t = Point.fangle    origin moved
        }

    let adjust_ipoint p t =
        Point.map int_of_float @@
        adjust_point ( Point.map float_of_int p ) t
end

module Entity : sig
    type t

    val create : Sprite.t -> t
    val rect : t -> Sdl.rect
    val position : t -> position
    val vector : t -> Vector.t
    val sprite : t -> Sprite.t
    val nudge : ?angle:float -> ?velocity:float -> t -> t
    val update : duration:float -> t -> t
    val render : Sdl.renderer -> t -> unit Sdl.result
end = struct

    type t =
        { x : float
        ; y : float
        ; speed : float
        ; sprite : Sprite.t
        ; a : float
        ; v : float
        ; t : float
        }

    let create sprite =
        { sprite
        ; x = 20.0
        ; y = 20.0
        ; speed = 1.0
        ; a = 0.0
        ; v = 0.0
        ; t = 0.0
        }

    let sprite t =
        t.sprite

    let position t =
        { Point.x = int_of_float t.x
        ; y = int_of_float t.y
        }

    let rect t =
        let w = Sprite.w t.sprite
        and h = Sprite.h t.sprite
        in
        Point.rect ~w ~h (position t)

    let vector t =
        Vector.create ~v:t.v ~t:t.t

    let nudge ?angle ?velocity t =
        let current =
            Vector.create ~t:t.t ~v:t.v
        and next =
            Vector.create
                ~t:(Option.default t.t angle)
                ~v:(Option.default 0.0 velocity)
        in
        let resultant =
            Vector.add current next
        in
        { t with
              v = Vector.v resultant
            ; t = Vector.t resultant
        }

    let update ~duration t =
        let dx = ( cos t.t ) *. t.v *. duration
        and dy = ( sin t.t ) *. t.v *. duration
        in
        let x = t.x +. dx
        and y = t.y +. dy
        in
        { t with x; y }

    let render renderer t =
        Sprite.render renderer (sprite t) (position t)
end


let flip_randomly tick s =
    if tick / 40 mod 2 = 0
    then Sprite.flip `Horizontal s
    else s

module Renderer : sig
    type t
    type colour = { r : int; g : int; b : int; a : int }

    val create : ?noalloc:bool -> Sdl.renderer -> t
    val rgba : r:int -> g:int -> b:int -> a:int -> colour
    val rgb  : r:int -> g:int -> b:int -> colour

    val white : colour
    val black : colour

    val clear : t -> t
    val set_draw_color : colour -> t -> t
    val draw_line : position -> position -> t -> t
    val draw_rect : Sdl.rect -> t -> t
    val draw_sprite : Sprite.t -> position -> t -> t
    val draw_entity : Entity.t -> t -> t

    val iter : ('a -> t -> t) -> 'a list -> t -> t
    val sometimes : bool -> ( t -> t ) -> t -> t
    val present : t -> unit Sdl.result
end = struct
    type runtime =
        | Deferred of unit Sdl.result Lazy.t list
        | Immediate of unit Sdl.result

    type t =
        { renderer : Sdl.renderer
        ; runtime : runtime
        }

    type colour = { r : int; g : int; b : int; a : int }

    let create ?(noalloc=false) renderer =
        let runtime =
            if noalloc
            then Immediate (Ok ())
            else Deferred []
        in
        { renderer; runtime }

    let rgba ~r ~g ~b ~a =
        { r; g; b; a }

    let rgb ~r ~g ~b =
        rgba ~r ~g ~b ~a:0xFF

    let black =
        rgb ~r:0x00 ~g:0x00 ~b:0x00

    let white =
        rgb ~r:0xFF ~g:0xFF ~b:0xFF

    let next_step t step =
        match t.runtime with
        | Deferred  steps -> { t with runtime = Deferred (step :: steps) }
        | Immediate sofar -> { t with runtime = Immediate (sofar >-> step) }

    let clear t =
        next_step t @@
        lazy ( Sdl.render_clear t.renderer )

    let set_draw_color c t =
        next_step t @@
        lazy ( Sdl.set_render_draw_color t.renderer c.r c.g c.b c.a )

    let draw_line a b t =
        next_step t @@
        lazy Point.( Sdl.render_draw_line t.renderer a.x a.y b.x b.y )

    let draw_rect rect t =
        next_step t @@
        lazy ( Sdl.render_draw_rect t.renderer ( Some rect ))

    let draw_sprite sprite pos t =
        next_step t @@
        lazy ( Sprite.render t.renderer sprite pos )

    let draw_entity entity t =
        next_step t @@
        lazy ( Entity.render t.renderer entity )

    let iter fn items t =
        List.fold_right (fun item t -> fn item t) items t

    let sometimes predicate f t =
        if predicate
        then f t
        else t

    let present t =
        match t.runtime with
        | Deferred steps ->
            let drawn =
                List.fold_right begin fun next sofar ->
                    sofar >-> next
                end steps
                ( Result.Ok () )
            in
            drawn >>| fun () -> Sdl.render_present t.renderer
        | Immediate sofar ->
            sofar >>| fun () -> Sdl.render_present t.renderer
end

let idle_sprite ?(speed=4) ss tick =
    ss.(tick / speed mod Array.length ss)

let random_array a =
    a.(Random.int @@ Array.length a)

type sprites =
    { character : sprite_set
    ; eye_demon : sprite_set
    ; red_patrol : sprite_set
    }

type spray =
    { sprite : sprite_set
    ; placement : position
    ; since : int
    }

type state =
    { mouse : position
    ; player : Entity.t
    ; sprites : sprites
    ; sprays : spray list
    ; z : int
    ; last_tick : int
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
    let {mouse;sprites;sprays;player} = state
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

    let mouse_sprite =
        idle_sprite sprites.character tick
    in
    let mouse_rect =
        Point.rect
            ~w:(Sprite.w mouse_sprite)
            ~h:(Sprite.h mouse_sprite)
            mouse
    and player_rect =
        Entity.rect player
    in

    Renderer.create ~noalloc:true r
    |> Renderer.set_draw_color black
    |> Renderer.clear
    |> Renderer.iter
        ( fun {sprite;placement;since} ->
            let frame =
                flip_randomly ( tick - since ) @@ idle_sprite ~speed:8 sprite tick
            in
            Renderer.draw_sprite frame placement
        )
        sprays
    |> Renderer.set_draw_color blue
    |> Renderer.draw_line
        center
        ( Vector.adjust_ipoint
            center
            ( Vector.scale 10.0 @@ Entity.vector player)
        )
    |> Renderer.draw_sprite
        (idle_sprite sprites.character tick) mouse
    |> Renderer.draw_entity player
    |> Renderer.draw_rect ( Entity.rect player )
    |> Renderer.sometimes
        ( Sdl.has_intersection mouse_rect player_rect )
        ( Renderer.set_draw_color red )
    |> Renderer.draw_rect
        ( Point.rect
            ~w:(Sprite.w sprites.character.(0)) 
            ~h:(Sprite.h sprites.character.(0)) 
            mouse
        )
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
    and eye_demon_sprites () =
        load_sprite "sprites/NPCs/eye-demon/idle1.png" >>= fun idle1 ->
        load_sprite "sprites/NPCs/eye-demon/idle2.png" >>= fun idle2 ->
        load_sprite "sprites/NPCs/eye-demon/idle3.png" >>= fun idle3 ->
        load_sprite "sprites/NPCs/eye-demon/idle4.png" >>| fun idle4 ->
        [| idle1
         ; idle2
         ; idle3
         ; idle4
        |]
    and red_patrol_sprites () =
        load_sprite "sprites/NPCs/red-patrol/vermelho1.png" >>= fun idle1 ->
        load_sprite "sprites/NPCs/red-patrol/vermelho2.png" >>= fun idle2 ->
        load_sprite "sprites/NPCs/red-patrol/vermelho3.png" >>| fun idle3 ->
        [| idle1
         ; idle2
         ; idle3
        |]
    in
    Sdl.render_set_scale renderer scale scale
    >>= character_sprites
    >>= fun character -> eye_demon_sprites ()
    >>= fun eye_demon -> red_patrol_sprites ()
    >>= fun red_patrol ->
    Rresult.R.ok
    { mouse =
        { x = 15
        ; y = 15
        }
    ; player =
        Entity.create character.(0)
    ; sprites =
        { character; eye_demon; red_patrol }
    ; sprays = []
    ; z = 0
    ; last_tick = 0
    }

let update_mouse ?(dx=0) ?(dy=0) mouse =
    let open Point in
    let x = mouse.x + dx
    and y = mouse.y + dy
    in
    { mouse with x; y }

module E = Sdl.Event

class common e =
    let enum      = E.(enum (get e typ))
    and timestamp = E.(get e timestamp) in
    object
        method enum      = enum
        method timestamp = timestamp
end

class keyboard e =
    let state    = E.(get e keyboard_state) in
    let scancode = E.(get e keyboard_scancode) in
    let keycode  = E.(get e keyboard_keycode) in
    let keymod   = E.(get e keyboard_keymod) in
    object
        inherit common e
        method window_id = E.(get e joy_axis_axis)
        method repeat    = E.(get e keyboard_repeat)
        method state     = state
        method scancode  = scancode
        method keycode   = keycode
        method keymod    = keymod
        method scancodee = Sdl.Scancode.enum scancode
end

type mouse_state =
    { button_mask : Sdl.uint32
    ; position : position
    }

type 'a our_event =
    | Tick of int
    | Mousemove of mouse_state
    | Keyboard of keyboard
    | Untouched of 'a

let ourify_event = function
    | `Mouse_motion, e ->
        let button_mask, (x, y) = Sdl.get_mouse_state () in
        let x, y = unscale (x, y) in
        Mousemove { button_mask; position = {x; y}}
    | `Key_down, e ->
        Keyboard (new keyboard e)
    | other -> Untouched other

let update state = function
    | Mousemove {position} ->
        { state with mouse = position }

    | Keyboard kb when kb#scancodee = `Right ->
        let velocity = 0.2 in
        { state with
            player = Entity.nudge ~angle:0.0 ~velocity state.player
        }

    | Keyboard kb when kb#scancodee = `Left ->
        let velocity = 0.2 in
        { state with
            player = Entity.nudge ~angle:pi ~velocity state.player
        }

    | Keyboard kb when kb#scancodee = `Up ->
        let velocity = 0.2 in
        { state with
            player = Entity.nudge ~angle:(pi /. -2.0) ~velocity state.player
        }

    | Keyboard kb when kb#scancodee = `Down ->
        let velocity = 0.2 in
        { state with
            player = Entity.nudge ~angle:(pi /. 2.0) ~velocity state.player
        }

    | Keyboard kb when kb#scancodee = `Space ->
        let {sprites;player;mouse} = state in
        (* let sprite    = random_sprite state.sprites.character *)
        let placement = mouse
        and sprite =
            random_array
            [| sprites.red_patrol
             ; sprites.character
             ; sprites.eye_demon
            |]
        and angle =
            Point.angle
                ( Entity.position player ) mouse
        and velocity =
            (fun x -> x /. 100.0) @@
            Point.distance
                ( Entity.position player ) mouse
        in
        { state with
            sprays =
                { sprite
                ; placement
                ; since = state.last_tick
                } :: state.sprays
          ; player = Entity.nudge ~angle ~velocity player
        }
    | Keyboard kb ->
        Sdl.log "keycode  : [ %d ] `%s`" kb#keycode @@ Sdl.get_key_name kb#keycode;
        Sdl.log "scancode : [ %d ] `%s`" kb#scancode @@ Sdl.get_scancode_name kb#scancode;
        state
    | Tick last_tick ->
        let player = Entity.update 1.0 state.player
        in
        { state with last_tick; player }
    | otherwise -> state

let run send ticks =
    let finished = ref false
    and t = ref 0
    and event = Sdl.Event.create ()
    and events = ref []
    in
    while not !finished do
        t      := 1 + !t;
        events := [ Tick !t ];
        while Sdl.poll_event (Some event) do
            match Sdl.Event.(enum (get event typ)) with
            | `Quit -> finished := true
            | e -> events := ourify_event (e, event) :: !events
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
