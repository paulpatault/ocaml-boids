type point =
  {
    mutable x : float;
    mutable y : float;
  }

let rand_point (boundx : float) (boundy : float) =
  {
    x=Random.float boundx;
    y=Random.float boundy;
  }

let op  f a b = { x = f a.x b.x; y = f a.y b.y }

let add   a b = op (+.) a b

let sub   a b = op (-.) a b

let scale a k = op ( *. ) a {x=k;y=k}

let div   a k = op ( /. ) a {x=k;y=k}

let dist  a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  sqrt (dx ** 2. +. dy ** 2.)

let (++.)   = add
let (--.)   = sub
let (//.)   = div
let ( **. ) = scale
