(* 0_Factorial.sml *)

(* int -> int 
 * Calculates the factorial of n. Tail recursive function, just because.
 *)

fun fact n =
    let
        fun helper (n, res) =
            case n of
             0 => res
            | _ => helper (n - 1, res * n)
    in
        helper (n, 1)
    end
