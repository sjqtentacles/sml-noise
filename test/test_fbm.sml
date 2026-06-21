(* test_fbm.sml -- fractal Brownian motion / turbulence helpers *)

structure FbmTests =
struct
  structure N = Noise
  open Support

  fun run () =
    let
      val basis = N.perlin2 ctx
      val one  = { octaves = 1, lacunarity = 2.0, gain = 0.5 } : N.fbmParams
      val four = { octaves = 4, lacunarity = 2.0, gain = 0.5 } : N.fbmParams
      val zero = { octaves = 0, lacunarity = 2.0, gain = 0.5 } : N.fbmParams

      val _ = Harness.section "fbm 1 octave = base noise"
      val pts = [(0.5,0.5),(1.3,2.7),(3.1,0.2),(5.5,6.6)]
      val () = Harness.check "fbm2 1 octave equals basis"
                 (List.all (fn p => close (N.fbm2 one basis p, basis p)) pts)

      val _ = Harness.section "fbm octave bound"
      (* with gain 0.5, summed amplitude < 2.0; |perlin| <= ~1, so |fbm| < ~2.1 *)
      val vals = sampleGrid (N.fbm2 four basis) (30, 0.41)
      val () = Harness.check "fbm2 within summed-amplitude bound"
                 (List.all (fn v => Real.abs v <= 2.1) vals)

      val _ = Harness.section "fbm zero octaves = 0"
      val () = checkClose "fbm2 0 octaves" (0.0, N.fbm2 zero basis (1.3, 2.7))
      val () = checkClose "turbulence2 0 octaves" (0.0, N.turbulence2 zero basis (1.3, 2.7))

      val _ = Harness.section "fbm reproducible per seed"
      val basis2 = N.perlin2 (N.fromSeed 0w20240621)
      val () = Harness.check "fbm2 reproducible"
                 (List.all (fn p => close (N.fbm2 four basis p, N.fbm2 four basis2 p)) pts)

      val _ = Harness.section "turbulence nonnegative-ish bound"
      (* turbulence sums |noise|, still bounded by summed amplitude *)
      val tvals = sampleGrid (N.turbulence2 four basis) (30, 0.41)
      val () = Harness.check "turbulence2 within bound"
                 (List.all (fn v => v >= 0.0 andalso v <= 2.1) tvals)
    in
      ()
    end
end
