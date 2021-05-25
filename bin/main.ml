open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js
open Dom_html
open Point
open Boids

let _ = Random.self_init ()

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
  ctx##setTransform 1. 0. 0. 1. 0. 0.;

  ctx##.strokeStyle := Js.string "#558cf466";
  ctx##beginPath;
  let hh = List.hd boid.history in
  ctx##moveTo hh.x hh.y;
  List.iter (fun point -> ctx##lineTo (point.x) (point.y)) boid.history;
  ctx##stroke;
  ()

let ( >>= ) = Lwt.bind

let rec animationLoop canvas =

  printBoids ();
  Lwt_js.sleep 0.01
  >>= fun () ->
    List.iter
      (
        fun boid ->
          flyTowardsCenter boid
          ; avoidOthers boid
          ; matchVelocity boid
          ; limitSpeed boid
          ; keepWithinBounds boid
          ; boid.position <- boid.position ++. boid.velocity
          ; majHistory boid
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
  let aaa = canvas ##. id in
  Printf.printf "%s\n" (Js.to_string aaa);

  ignore (animationLoop canvas);
  Js._false

let _ = Html.window##.onload := Html.handler start
