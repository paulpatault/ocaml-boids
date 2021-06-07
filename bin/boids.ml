open Point

let width = 900.
let height = 600.
let prof = 600.
let numBoids = ref 100
let visualRange = 75.
let margin = 150.
let centeringFactor = ref 0.015
let avoidFactor = ref 0.015
let matchingFactor = ref 0.2
let speedLimit = ref 5.

type boid =
  {
    mutable position : point;
    mutable velocity : point;
    mutable history : point list;
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
  for _ = 0 to !numBoids - 1 do
    let position = rand_point width height prof
    in let velocity = sub (rand_point 10. 10. 10.) {x=5.; y=5.; z=5.}
    (* in boids := {position=position; velocity=velocity} :: !boids; *)
    in boids := {position=position; velocity=velocity; history=[]} :: !boids;
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

let flyTowardsCenter boid =
  let center = ref {x=0.;y=0.;z=0.} in
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
      boid.velocity <- boid.velocity ++. ((!center --. boid.position) **. !centeringFactor);
    end

let avoidOthers boid =
  let minDistance = 20. in
  let move = ref {x=0.;y=0.;z=0.} in
  List.iter
    ( fun otherBoid ->
        if otherBoid <> boid then begin
          let distance = dist otherBoid.position boid.position in
          if distance < minDistance
          then move := !move ++. (boid.position --. otherBoid.position);
        end)
    !boids;
  boid.velocity <- boid.velocity ++. (!move **. !avoidFactor)

let matchVelocity boid =
  let avgVelocity = ref {x=0.;y=0.;z=0.} in
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
  if !nbNeighbours > 0 then
    begin
      avgVelocity := !avgVelocity //. (float_of_int !nbNeighbours);
      let diff = !avgVelocity --. boid.velocity in
      let d' = diff **. !matchingFactor in
      boid.velocity <- boid.velocity ++. d'
    end

let limitSpeed boid =
  let speed = sqrt (boid.velocity.x ** 2. +. boid.velocity.y ** 2. +. boid.velocity.z ** 2.) in
  if speed > !speedLimit then
    boid.velocity <- boid.velocity **. (!speedLimit /. speed)

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
  boid.history <- boid.position :: boid.history;
  boid.history <- resize boid.history
