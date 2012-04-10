type scalar = float
type point = scalar * scalar
type vector = point

let s_plus = (+.)

let s_minus = (-.)

let s_times = ( *. )

let s_divide = ( /. )

let s_dist (a,b) (c,d) = 
  let e = s_minus d b and
      f = s_minus c a in
  let g = s_plus (s_times e e) (s_times f f) in
  sqrt g

let s_compare a b = if a > b then 1 else if a = b then 0 else -1

let s_to_string = string_of_float

let point_to_string (x,y) = 
  "("^(s_to_string x)^", "^(s_to_string y)^")"

let v_plus (a,b)(c,d) = (s_plus a c, s_plus b d)

let distance = s_dist

let midpoint (a,b) (c,d) = 
  (s_divide (s_plus c a) 2., s_divide (s_plus d b) 2.)

let head (a,b) = (abs_float a, abs_float b)

let sum f s = Sequence.map_reduce f (0.,0.) v_plus s

let scale_point s (x,y) = (s_times s x, s_times s y)

let unit_vector (a,b) (c,d) = 
  let mag = distance (c,d) (a,b) in
  (s_divide (s_minus c a) mag, s_divide (s_minus d b) mag)
