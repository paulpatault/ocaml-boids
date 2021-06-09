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

let mk_color_from_z z =
  let color = z /. 600. *. 50. +. 25. in
  let color_str = "HSL(219,88\u{0025}," ^ (string_of_float color) ^ "\u{0025})" in
  Js.string color_str

let mk_color_from_z z =
  let hue = 219 in
  let sat = 88 in
  let light = (int_of_float z) / 600 * 50 + 25 in
  let color_str =
    "HSL(" ^ string_of_int hue ^ ","
    ^ string_of_int sat ^ "\u{0025},"
    ^ string_of_int light ^ "\u{0025})"
  in Js.string color_str

let drawBoid boid canvas =
  let ctx = canvas##getContext Html._2d_ in
  let angle = atan2 boid.velocity.y boid.velocity.x in
  Printf.printf "%3f\n" boid.position.z;
  let dx, dy =
    boid.position.z /. 900. *. 15. +. 5.,
    boid.position.z /. 600. *. 5. +. 1.5
  in

  ctx##beginPath;
  let first = Queue.peek boid.history in
  ctx##moveTo first.x first.y;
  Queue.iter
    (fun point ->
      (* ctx##.strokeStyle := mk_color_from_z point.z true; *)
      ctx##.strokeStyle := Js.string "#558cf466";
      ctx##lineTo point.x point.y) 
    boid.history;
  ctx##stroke;
  ctx##closePath;

  ctx##translate boid.position.x boid.position.y;
  ctx##rotate angle;
  ctx##translate (-. boid.position.x) (-. boid.position.y);
  ctx##beginPath;
  ctx##moveTo boid.position.x boid.position.y;
  ctx##lineTo (boid.position.x -. dx) (boid.position.y +. dy);
  ctx##lineTo (boid.position.x -. dx) (boid.position.y -. dy);
  ctx##lineTo boid.position.x boid.position.y;
  ctx##closePath;
  ctx##.fillStyle := mk_color_from_z boid.position.z;
  (* ctx##.fillStyle := Js.string "#558cf4"; *)
  ctx##fill;
  ctx##setTransform 1. 0. 0. 1. 0. 0.


let ( >>= ) = Lwt.bind

let rec animationLoop canvas =
  Lwt_js.sleep 0.01
  >>= fun () ->
    List.iter
      (fun boid ->
          fusion boid
          ; limitSpeed boid
          ; keepWithinBounds boid
          ; onMoreStep boid)
      !boids;
    let ctx = canvas##getContext Html._2d_ in
    ctx##clearRect 0. 0. width height;
    List.iter (fun boid -> drawBoid boid canvas) !boids;
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
