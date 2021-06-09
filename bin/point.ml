type point =
  {
    mutable x : float;
    mutable y : float;
    mutable z : float;
  }

let rand_point (boundx : float) (boundy : float) (boundz : float) =
  {
    x=Random.float boundx;
    y=Random.float boundy;
    z=Random.float boundz;
  }

let op  f a b = { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z }

let add   a b = op (+.) a b

let sub   a b = op (-.) a b

let scale a k = op ( *. ) a {x=k;y=k;z=k}

let div   a k = op ( /. ) a {x=k;y=k;z=k}

let dist  a b =
  let dx = a.x -. b.x in
  let dy = a.y -. b.y in
  let dz = a.z -. b.z in
  sqrt (dx ** 2. +. dy ** 2. +. dz ** 2.)

let (++.)   = add
let (--.)   = sub
let (//.)   = div
let ( **. ) = scale
