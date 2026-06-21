(* sml-noise demo: samples fractal Brownian motion (fBm over Perlin noise) on a
   grid, normalizes it, applies a terrain colormap, and writes assets/terrain.png. *)

val width = 512
val height = 512

val ctx = Noise.fromSeed 0wxC0FFEE15600D
val params : Noise.fbmParams = { octaves = 6, lacunarity = 2.0, gain = 0.5 }
fun basis (x, y) = Noise.perlin2 ctx (x, y)
val scale = 1.0 / 110.0

(* Precompute one height per pixel so fBm is not recomputed per channel. *)
val heights = Array.array (width * height, 0.0)
val () =
  let
    fun loop i =
      if i >= width * height then ()
      else
        let
          val x = i mod width
          val y = i div width
          val v = Noise.fbm2 params basis (real x * scale, real y * scale)
        in
          Array.update (heights, i, v);
          loop (i + 1)
        end
  in
    loop 0
  end

(* Normalize to [0, 1] from the observed range. *)
val (lo, hi) =
  Array.foldl (fn (v, (mn, mx)) =>
                  (if v < mn then v else mn, if v > mx then v else mx))
              (Real.posInf, Real.negInf) heights
val span = if Real.== (hi, lo) then 1.0 else hi - lo

fun clamp01 v = if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v

(* Piecewise-linear terrain colormap: water -> sand -> grass -> rock -> snow. *)
val stops =
  [ (0.00, (18.0,  36.0,  82.0))
  , (0.42, (28.0,  88.0, 150.0))
  , (0.48, (46.0, 132.0, 182.0))
  , (0.50, (210.0, 200.0, 140.0))
  , (0.56, (72.0, 150.0,  66.0))
  , (0.72, (40.0, 100.0,  46.0))
  , (0.84, (112.0, 102.0,  96.0))
  , (0.93, (236.0, 236.0, 242.0))
  , (1.00, (255.0, 255.0, 255.0)) ]

fun colormap t =
  let
    fun go ((t0, c0) :: (t1, c1) :: rest) =
          if t <= t1 then
            let
              val f = if Real.== (t1, t0) then 0.0 else (t - t0) / (t1 - t0)
              val (r0, g0, b0) = c0
              val (r1, g1, b1) = c1
            in
              (r0 + (r1 - r0) * f, g0 + (g1 - g0) * f, b0 + (b1 - b0) * f)
            end
          else go ((t1, c1) :: rest)
      | go [(_, c)] = c
      | go [] = (0.0, 0.0, 0.0)
  in
    go stops
  end

val data = Word8Vector.tabulate (4 * width * height, fn i =>
  let
    val px = i div 4
    val ch = i mod 4
    val t = clamp01 ((Array.sub (heights, px) - lo) / span)
    val (r, g, b) = colormap t
  in
    case ch of
        0 => Word8.fromInt (Real.round r)
      | 1 => Word8.fromInt (Real.round g)
      | 2 => Word8.fromInt (Real.round b)
      | _ => 0w255
  end)

val img : Image.image = { width = width, height = height, data = data }

val () =
  let
    val os = BinIO.openOut "assets/terrain.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/terrain.png\n"
  end
