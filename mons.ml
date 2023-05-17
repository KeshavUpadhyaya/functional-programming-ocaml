type ptype = TNormal | TFire | TWater

type peff  = ENormal | ENotVery | ESuper

let mult_of_eff = function 
  | ENormal -> 1.0
  | ENotVery -> 0.5
  | ESuper -> 2.0


let eff = function a -> 2
