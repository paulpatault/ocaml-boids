open Point

let width = 900.
let height = 600.
let numBoids = 100
let visualRange = 75.

type boid =
  {
    mutable position : point;
    mutable velocity : point;
    mutable history  : point Queue.t;
  }

let boids : boid list ref = ref []

let printBoids () =
  let rec aux i = function
    | [] -> ()
    | e::k ->
      Printf.printf
        "%d: pos=(%.2f, %.2f), vel=(%.2f, %.2f)" i
        e.position.x e.position.y
        e.velocity.x e.velocity.y
      ; aux (i+1) k
  in aux 0 !boids

let initBoids () =
  for _ = 0 to numBoids - 1 do
    let position = rand_point width height
    in let velocity = sub (rand_point 10. 10.) {x=5.; y=5.}
    in boids := {position=position; velocity=velocity; history=Queue.create ()} :: !boids;
  done

let keepWithinBounds boid =
  let margin = 150. in
  let turnFactor = 1. in

  if boid.position.x < margin
    then boid.velocity.x <- boid.velocity.x +. turnFactor;
  if boid.position.x > width -. margin
    then boid.velocity.x <- boid.velocity.x -. turnFactor;

  if boid.position.y < margin
    then boid.velocity.y <- boid.velocity.y +. turnFactor;
  if boid.position.y > height -. margin
    then boid.velocity.y <- boid.velocity.y -. turnFactor;
  ()

let flyTowardsCenter boid =
  let centeringFactor = 0.015 in
  let center = ref {x=0.;y=0.} in
  let nbNeighbours = ref 0 in
  List.iter
    ( fun otherBoid ->
        let distance = dist otherBoid.position boid.position in
        if distance < visualRange
        then begin
          center := !center ++. otherBoid.position;
          incr nbNeighbours
        end)
    !boids;
  if !nbNeighbours > 0 then
    begin
      center := !center //. (float_of_int !nbNeighbours);
      boid.velocity <- boid.velocity ++. ((!center --. boid.position) **. centeringFactor);
    end

let avoidOthers boid =
  let minDistance = 20. in
  let avoidFactor = 0.015 in
  let move = ref {x=0.;y=0.} in
  List.iter
    ( fun otherBoid ->
        if otherBoid <> boid then begin
          let distance = dist otherBoid.position boid.position in
          if distance < minDistance
          then move := !move ++. (boid.position --. otherBoid.position);
        end)
    !boids;
  boid.velocity <- boid.velocity ++. (!move **. avoidFactor)

let matchVelocity boid =
  let avgVelocity = ref {x=0.;y=0.} in
  let nbNeighbours = ref 0 in
  List.iter
    ( fun otherBoid ->
          let distance = dist otherBoid.position boid.position in
          if distance < visualRange
          then begin
              avgVelocity := !avgVelocity ++. otherBoid.velocity;
              incr nbNeighbours
          end)
    !boids;
  let matchingFactor = 0.05 in
  if !nbNeighbours > 0 then
    begin
      avgVelocity := !avgVelocity //. (float_of_int !nbNeighbours);
      let diff = !avgVelocity --. boid.velocity in
      let d' = diff **. matchingFactor in
      boid.velocity <- boid.velocity ++. d'
    end

let limitSpeed boid =
  let speedLimit = 10. in
  let speed = sqrt (boid.velocity.x ** 2. +. boid.velocity.y ** 2.) in
  if speed > speedLimit then
    boid.velocity <- boid.velocity **. (speedLimit /. speed)

let resize l =
  let rec aux acc l' =
    match l' with
    | [] -> acc
    | _::[] -> acc
    | e::k -> aux (e :: acc) k
  in
  if List.length l > 50 then List.rev (aux [] l)
  else l

let onMoreStep boid =
  boid.position <- boid.position ++. boid.velocity;

  Queue.add boid.position boid.history;

  if Queue.length boid.history > 50
  then let _ = Queue.take boid.history
  in ()
