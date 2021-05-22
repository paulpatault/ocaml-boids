open Point

let _ = Random.self_init ()

let width = 500.
let height = 500.
let numBoids = 5
let visualRange = 75.

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
  for _ = 0 to numBoids - 1 do
    let position = rand_point width height
    in let velocity = sub (rand_point 10. 10.) {x=5.; y=5.}
    in boids := {position=position; velocity=velocity; history=[]} :: !boids;
  done

let compare boid boid1 boid2 =
  let d1 = dist boid.position boid1.position
  in let d2 = dist boid.position boid2.position
  in int_of_float (d1 -. d2)

let subList l i j =
  let rec aux l k acc =
    if k = j then acc
    else aux (List.tl l) (k+1) (List.hd l :: acc)
  in aux l i []

let nClosestBoids boid n =
  let ccb = compare boid in
  let sorted = List.sort ccb (!boids) in
  subList sorted 1 (n + 1)

let keepWithinBounds boid =
  let margin = 150. in
  let turnFactor = 2. in

  if boid.position.x < margin then
    boid.velocity.x <- boid.velocity.x +. turnFactor;

  if boid.position.x > width -. margin then
    boid.velocity.x <- boid.velocity.x -. turnFactor;

  if boid.position.y < margin then
    boid.velocity.y <- boid.velocity.y +. turnFactor;

  if boid.position.y > height -. margin then
    boid.velocity.y <- boid.velocity.y -. turnFactor

let flyTowardsCenter boid =

  let centeringFactor = 0.005 in

  let center = {x=0.;y=0.} in
  let nbNeighboords = ref 0 in

  List.iter
    (
      fun e ->
        if e <> boid then
        let distance = dist e.position boid.position in
        if distance < visualRange
        then
          begin
            center.x <- center.x +. e.position.x;
            center.y <- center.y +. e.position.y;
            incr nbNeighboords;
          end
    )
    !boids;


  if !nbNeighboords > 0 then
    begin
      center.x <- center.x /. (float_of_int !nbNeighboords);
      center.y <- center.y /. (float_of_int !nbNeighboords);
      boid.velocity.x <- (center.x -. boid.velocity.x) *. centeringFactor;
      boid.velocity.y <- (center.y -. boid.velocity.y) *. centeringFactor;
    end

let avoidOthers boid =
  let minDistance = 20. in
  let avoidFactor = 0.05 in
  let move = {x=0.;y=0.} in

  List.iter
    (
      fun e ->
        if e <> boid then
        let distance = dist e.position boid.position in
        if distance < minDistance
        then
          begin
            move.x <- move.x +. boid.position.x -. e.position.x;
            move.y <- move.y +. boid.position.y -. e.position.y;
          end
    )
    !boids;

  boid.velocity <- add boid.velocity (scale move avoidFactor)

let matchVelocity boid =

  let averageDistance:point = {x=0.;y=0.} in
  let nbn = ref 0 in

  List.iter
    (
      fun e ->
          let distance = dist e.position boid.position in
          if distance < visualRange
          then
            begin
              averageDistance.x <- averageDistance.x +. e.velocity.x;
              averageDistance.y <- averageDistance.y +. e.velocity.y;
              incr nbn
            end
    )
    !boids;

  let matchingFactor = 0.05 in

  if !nbn > 0 then
    begin
      averageDistance.x <- averageDistance.x /. (float_of_int !nbn);
      averageDistance.y <- averageDistance.y /. (float_of_int !nbn);
      boid.velocity <- add boid.velocity (scale (sub averageDistance boid.velocity) matchingFactor);
    end

let limitSpeed boid =
  let speedLimit = 15. in

  let speed = sqrt (boid.velocity.x ** 2. +. boid.velocity.y ** 2.) in

  if speed > speedLimit then
    boid.velocity <- scale boid.velocity (speedLimit /. speed)

