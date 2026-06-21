(* support.sml -- shared helpers for noise tests. *)

structure Support =
struct
  structure N = Noise

  val eps = 1E~9
  fun close (a, b) = Real.abs (a - b) <= eps
  fun checkClose name (exp, act) = Harness.check name (close (exp, act))
  fun checkNear name tol (exp, act) =
    Harness.check name (Real.abs (exp - act) <= tol)

  (* sample a function over a fixed grid and return all results *)
  fun sampleGrid f (n, step) =
    let
      val acc = ref ([] : real list)
      fun loop (i, j) =
        if i >= n then ()
        else if j >= n then loop (i+1, 0)
        else ( acc := f (Real.fromInt i * step, Real.fromInt j * step) :: !acc
             ; loop (i, j+1) )
    in
      loop (0, 0); !acc
    end

  val ctx = N.fromSeed 0w20240621
end
