(* 1_Pascal.sml *)
    
(* int list -> int list
 * Helper function, shifts the row (list) to the right, by adding 0 to the front of the list.
 * [1,3,3,1] becomes [0,1,3,3,1]
 *)

fun ShiftRight lst =
    0::lst

(* int list -> int list
 * Helper function, shifts the row (list) to the left, by adding 0 to the end of the list.
 * [1,3,3,1] becomes [1,3,3,1,0]
 *)

fun ShiftLeft lst =
    case lst of
     [] => [0]
    | _ => List.rev (ShiftRight lst)
    

(* int list * int list -> int list
 * Adds one list shifted to the left and one to the right.
 *)

fun AddList (lstL,lstR) =
    case lstL of
     x::xs' => (case lstR of
                 y::ys' => (x+y)::AddList(xs',ys')
                |_ => [])
    | _     => []    

(* int -> int list
 * Calculates the nth row of Pascal's triangle.
 *)

 fun Pascal N =
     case N of
      1 => [1]
     |_ => let
               val L = Pascal (N - 1)
           in
               AddList ((ShiftLeft L, ShiftRight L))
           end
