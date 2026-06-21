(* noise.sig

   Procedural noise functions built on sml-glm vectors and a seeded
   permutation table (derived from sml-prng). All functions are pure,
   total, and deterministic for a given seed, and byte-identical across
   MLton and Poly/ML.

   Conventions:
   - A `Noise.t` bundles the seeded permutation/gradient tables; build one
     with `fromSeed` and reuse it across samples.
   - Perlin and simplex noise are signed, roughly in [-1, 1]; value noise is
     in [0, 1]; Worley returns nonnegative distances. *)

signature NOISE =
sig
  type t

  (* Build a noise context from a 64-bit seed. *)
  val fromSeed : Word64.word -> t

  (* Classic Perlin gradient noise. Zero at integer lattice points. *)
  val perlin2 : t -> real * real -> real
  val perlin3 : t -> real * real * real -> real

  (* Simplex noise (Ken Perlin's improved variant). *)
  val simplex2 : t -> real * real -> real
  val simplex3 : t -> real * real * real -> real

  (* Value noise in [0, 1] (smoothstep-interpolated lattice values). *)
  val value2 : t -> real * real -> real
  val value3 : t -> real * real * real -> real

  (* Worley / cellular noise. Returns (F1, F2): the distances to the nearest
     and second-nearest feature points (Euclidean). *)
  val worley2 : t -> real * real -> real * real

  (* Fractal Brownian motion / turbulence over any 2D noise basis.
     `octaves` >= 0; `lacunarity` ~ 2.0; `gain`/persistence ~ 0.5. *)
  type fbmParams = { octaves : int, lacunarity : real, gain : real }
  val fbm2        : fbmParams -> (real * real -> real) -> real * real -> real
  val turbulence2 : fbmParams -> (real * real -> real) -> real * real -> real
end
