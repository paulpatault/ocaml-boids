open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js
open Dom_html
open Graphics
open Point
open Boids

module Html = Dom_html

let error f =
  Printf.ksprintf
    (fun s ->
      Firebug.console##error (Js.string s);
      failwith s)
    f

let drawBoid boid ctx =

  let angle = atan2 boid.velocity.y boid.velocity.x in
  ctx##translate boid.position.x boid.position.y;
  ctx##rotate angle;
  ctx##translate (-. boid.position.x) (-. boid.position.y);
  ctx##.fillStyle := Js.string "#558cf4";
  ctx##beginPath;
  ctx##moveTo boid.position.x boid.position.y;
  ctx##lineTo (boid.position.x -. 15.) (boid.position.y +. 5.);
  ctx##lineTo (boid.position.x -. 15.) (boid.position.y -. 5.);
  ctx##lineTo boid.position.x boid.position.y;
  ctx##fill;
  ctx##setTransform 1. 0. 0. 1. 0. 0.

let ( >>= ) = Lwt.bind

let rec animationLoop canvas =

  printBoids ();
  Lwt_js.sleep 0.1
  >>= fun () ->
    List.iter
      (
        fun e ->
          flyTowardsCenter e
          ; avoidOthers e
          ; matchVelocity e
          ; limitSpeed e
          ; keepWithinBounds e
          ; e.position <- add e.position e.velocity
          (* ; e.history <- e.position :: e.history *)
          (* boid.history = boid.history.slice(-50); *)
      )
      !boids;
    let ctx = canvas##getContext Html._2d_ in
    ctx##clearRect 0. 0. width height;
    List.iter (fun e -> drawBoid e ctx) !boids;
    animationLoop canvas

let start _ =
  initBoids ();
  let canvas =
    Opt.get
        (Opt.bind
           (Dom_html.document##getElementById (string "boids"))
           Dom_html.CoerceTo.canvas)
        (fun () -> error "can't find canvas element %s" "boids")
  in
  ignore (animationLoop canvas);
  Js._false

let _ = Html.window##.onload := Html.handler start
