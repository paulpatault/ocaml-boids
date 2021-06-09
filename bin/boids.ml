open Point

let width = 900.
let height = 600.
let prof = 600.
let visualRange = 75.
let margin = 150.
let numBoids = ref 100
let speedLimit = ref 7.
let minDistance = ref 20.
let centeringFactor = ref 0.015
let avoidFactor = ref 0.015
let matchingFactor = ref 0.05

type boid =
  {
    mutable position : point;
    mutable velocity : point;
    mutable history  : point Queue.t;
  }

let boids : boid list ref = ref []

(* let printBoids () =
  let rec aux i = function
    | [] -> ()
    | e::k ->
      Printf.printf
        "%d: pos=(%.2f, %.2f), vel=(%.2f, %.2f)" i
        e.position.x e.position.y
        e.velocity.x e.velocity.y
      ; aux (i+1) k
  in aux 0 !boids *)

let initBoids () =
  for _ = 0 to !numBoids - 1 do
    let position = rand_point width height prof
    in let velocity = sub (rand_point 10. 10. 10.) {x=5.; y=5.; z=5.}
    in boids := {position=position; velocity=velocity; history=Queue.create ()} :: !boids;
  done

let keepWithinBounds boid =
  let turnFactor = 1. in

  if boid.position.x < margin
    then boid.velocity.x <- boid.velocity.x +. turnFactor;
  if boid.position.x > width -. margin
    then boid.velocity.x <- boid.velocity.x -. turnFactor;

  if boid.position.y < margin
    then boid.velocity.y <- boid.velocity.y +. turnFactor;
  if boid.position.y > height -. margin
    then boid.velocity.y <- boid.velocity.y -. turnFactor;

  if boid.position.z < margin
    then boid.velocity.z <- boid.velocity.z +. turnFactor;
  if boid.position.z > height -. margin
    then boid.velocity.z <- boid.velocity.z -. turnFactor;
  ()

let fusion boid =
  let center = ref {x=0.;y=0.;z=0.} in
  let move = ref {x=0.;y=0.;z=0.} in
  let avgVelocity = ref {x=0.;y=0.;z=0.} in
  let nbNeighbours = ref 0 in

  List.iter
    ( fun otherBoid ->
        let distance = dist otherBoid.position boid.position in
        if distance < visualRange
        then begin
          incr nbNeighbours;
          (* flyTowardsCenter *)
          center := !center ++. otherBoid.position;
          (* matchVelocity *)
          avgVelocity := !avgVelocity ++. otherBoid.velocity
        end;
        if otherBoid <> boid then begin
          if distance < !minDistance
          (* avoidOthers *)
          then move := !move ++. (boid.position --. otherBoid.position);
        end)
    !boids;

  if !nbNeighbours > 0 then
    begin
      (* flyTowardsCenter *)
      center := !center //. (float_of_int !nbNeighbours);
      (* matchVelocity *)
      boid.velocity <- boid.velocity ++. ((!center --. boid.position) **. !centeringFactor);
      avgVelocity := !avgVelocity //. (float_of_int !nbNeighbours);
      (* avoidOthers *)
      let diff = !avgVelocity --. boid.velocity in
      let d' = diff **. !matchingFactor in
      boid.velocity <- boid.velocity ++. d'
    end;
  boid.velocity <- boid.velocity ++. (!move **. !avoidFactor)

let limitSpeed boid =
  let speedLimit = 6. in
  let speed = sqrt (boid.velocity.x ** 2. +. boid.velocity.y ** 2. +. boid.velocity.z ** 2.) in
  if speed > speedLimit
  then boid.velocity <- boid.velocity **. (speedLimit /. speed)

let onMoreStep boid =
  boid.position <- boid.position ++. boid.velocity;
  Queue.add boid.position boid.history;

  if Queue.length boid.history > 50
  then let _ = Queue.take boid.history
  in ()
