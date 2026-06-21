(* test_value_worley.sml -- value noise and Worley/cellular noise *)

structure ValueWorleyTests =
struct
  structure N = Noise
  open Support

  fun run () =
    let
      val _ = Harness.section "value noise range [0,1]"
      val v2 = sampleGrid (N.value2 ctx) (40, 0.37)
      val () = Harness.check "value2 in [0,1]"
                 (List.all (fn v => v >= 0.0 andalso v <= 1.0) v2)
      val v3 = sampleGrid (fn (x,y) => N.value3 ctx (x, y, 0.3)) (30, 0.41)
      val () = Harness.check "value3 in [0,1]"
                 (List.all (fn v => v >= 0.0 andalso v <= 1.0) v3)
      (* lattice points equal the stored lattice value (exactly reproducible) *)
      val () = Harness.check "value2 reproducible"
                 (close (N.value2 ctx (2.5, 3.5),
                         N.value2 (N.fromSeed 0w20240621) (2.5, 3.5)))

      val _ = Harness.section "Worley F1 <= F2, nonneg"
      val pts = [(0.3,0.7),(1.5,2.5),(3.14,2.71),(~1.2,4.4),(10.5,~3.3)]
      val () = Harness.check "worley F1 <= F2"
                 (List.all (fn p => let val (f1,f2) = N.worley2 ctx p in f1 <= f2 end) pts)
      val () = Harness.check "worley distances nonnegative"
                 (List.all (fn p => let val (f1,f2) = N.worley2 ctx p
                                    in f1 >= 0.0 andalso f2 >= 0.0 end) pts)

      val _ = Harness.section "Worley F1 = 0 at a feature point"
      (* sampling exactly at a cell's feature point gives F1 ~ 0 *)
      val (fx, fy) =
        let
          (* reconstruct cell (0,0)'s feature point the same way the impl does
             by sampling very close and trusting monotonic distance; instead we
             just check that the minimum over a fine scan near a cell is ~0 *)
          val best = ref 1.0E30
          fun scan (i, j) =
            if i > 100 then ()
            else if j > 100 then scan (i+1, 0)
            else let val x = Real.fromInt i / 100.0
                     val y = Real.fromInt j / 100.0
                     val (f1, _) = N.worley2 ctx (x, y)
                 in (if f1 < !best then best := f1 else ()); scan (i, j+1) end
        in scan (0,0); (!best, 0.0) end
      val () = Harness.check "worley F1 reaches ~0 near its feature point"
                 (fx < 0.02)

      val _ = Harness.section "Worley F1 grows away from feature point"
      (* take a point, then a point shifted far; min distance shouldn't be
         systematically tiny everywhere -> at least some F1 is sizeable *)
      val f1s = List.map (fn p => #1 (N.worley2 ctx p)) pts
      val () = Harness.check "worley has some sizeable F1"
                 (List.exists (fn d => d > 0.1) f1s)
    in
      ()
    end
end
