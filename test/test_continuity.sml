(* test_continuity.sml -- finite-difference continuity guard.

   A tiny step in input must produce a small change in output. This catches
   lattice indexing / permutation-wrap bugs that would otherwise show up as
   discontinuous jumps at cell boundaries. *)

structure ContinuityTests =
struct
  structure N = Noise
  open Support

  fun run () =
    let
      val h = 1.0E~4
      (* maximum allowed change for a step of size h: Perlin's gradient is
         bounded, so |df| <= L*h for a modest Lipschitz constant L. *)
      val bound = 0.05

      fun maxDelta f =
        let
          val acc = ref 0.0
          fun loop (i, j) =
            if i >= 30 then ()
            else if j >= 30 then loop (i+1, 0)
            else
              let
                val x = Real.fromInt i * 0.33 + 0.07
                val y = Real.fromInt j * 0.33 + 0.07
                val v0 = f (x, y)
                val dx = Real.abs (f (x + h, y) - v0)
                val dy = Real.abs (f (x, y + h) - v0)
                val m = Real.max (dx, dy)
              in
                (if m > !acc then acc := m else ()); loop (i, j+1)
              end
        in
          loop (0, 0); !acc
        end

      val _ = Harness.section "perlin2 continuity"
      val () = Harness.check "perlin2 small step -> small change"
                 (maxDelta (N.perlin2 ctx) < bound)

      val _ = Harness.section "value2 continuity"
      val () = Harness.check "value2 small step -> small change"
                 (maxDelta (N.value2 ctx) < bound)

      val _ = Harness.section "simplex2 continuity"
      val () = Harness.check "simplex2 small step -> small change"
                 (maxDelta (N.simplex2 ctx) < bound)

      val _ = Harness.section "simplex range sanity"
      val sv = sampleGrid (N.simplex2 ctx) (40, 0.37)
      val () = Harness.check "simplex2 within [-1.1,1.1]"
                 (List.all (fn v => v >= ~1.1 andalso v <= 1.1) sv)
      val sv3 = sampleGrid (fn (x,y) => N.simplex3 ctx (x,y,0.4)) (25, 0.41)
      val () = Harness.check "simplex3 within [-1.1,1.1]"
                 (List.all (fn v => v >= ~1.1 andalso v <= 1.1) sv3)
    in
      ()
    end
end
