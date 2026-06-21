(* test_perlin.sml -- Perlin gradient noise properties *)

structure PerlinTests =
struct
  structure N = Noise
  open Support

  fun run () =
    let
      val _ = Harness.section "perlin2 lattice / range"
      (* zero at integer lattice points *)
      val () = checkClose "perlin2 (0,0) = 0" (0.0, N.perlin2 ctx (0.0, 0.0))
      val () = checkClose "perlin2 (3,5) = 0" (0.0, N.perlin2 ctx (3.0, 5.0))
      val () = checkClose "perlin2 (-2,7) = 0" (0.0, N.perlin2 ctx (~2.0, 7.0))
      (* within the theoretical bound (|perlin2| <= ~1.0; allow small slack) *)
      val vals = sampleGrid (N.perlin2 ctx) (40, 0.37)
      val () = Harness.check "perlin2 within [-1.05,1.05]"
                 (List.all (fn v => v >= ~1.05 andalso v <= 1.05) vals)
      (* not all zero (sanity) *)
      val () = Harness.check "perlin2 is nontrivial"
                 (List.exists (fn v => Real.abs v > 0.01) vals)

      val _ = Harness.section "perlin3 lattice / range"
      val () = checkClose "perlin3 (0,0,0) = 0" (0.0, N.perlin3 ctx (0.0,0.0,0.0))
      val () = checkClose "perlin3 (2,3,4) = 0" (0.0, N.perlin3 ctx (2.0,3.0,4.0))
      val vals3 = sampleGrid (fn (x,y) => N.perlin3 ctx (x, y, 0.5)) (30, 0.41)
      val () = Harness.check "perlin3 within [-1.05,1.05]"
                 (List.all (fn v => v >= ~1.05 andalso v <= 1.05) vals3)

      val _ = Harness.section "seed sensitivity / reproducibility"
      val c1 = N.fromSeed 0w1
      val c2 = N.fromSeed 0w2
      val () = Harness.check "same seed, same value"
                 (close (N.perlin2 c1 (1.3, 2.7), N.perlin2 (N.fromSeed 0w1) (1.3, 2.7)))
      val () = Harness.check "different seeds differ somewhere"
                 (List.exists
                    (fn (x,y) => not (close (N.perlin2 c1 (x,y), N.perlin2 c2 (x,y))))
                    [(0.5,0.5),(1.3,2.7),(3.1,0.2),(5.5,6.6),(0.1,9.9)])

      val _ = Harness.section "perlin3 reduces to perlin2 on a plane (sanity)"
      (* at fixed integer z, perlin3 is still a valid 2D field; just assert
         it is continuous & bounded (not a hard equality to perlin2) *)
      val planar = sampleGrid (fn (x,y) => N.perlin3 ctx (x, y, 0.0)) (20, 0.5)
      val () = Harness.check "perlin3 plane bounded"
                 (List.all (fn v => v >= ~1.05 andalso v <= 1.05) planar)

      val _ = Harness.section "perlin edge: large & negative coords"
      val () = Harness.check "perlin2 large coords bounded"
                 (let val v = N.perlin2 ctx (12345.6, ~98765.4)
                  in v >= ~1.05 andalso v <= 1.05 end)
      val () = Harness.check "perlin3 large coords bounded"
                 (let val v = N.perlin3 ctx (~50000.5, 40000.25, 12345.75)
                  in v >= ~1.05 andalso v <= 1.05 end)
    in
      ()
    end
end
