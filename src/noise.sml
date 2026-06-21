(* noise.sml

   Implementation of NOISE. The permutation table is a Fisher-Yates shuffle of
   0..255 (doubled to 512 to avoid index wrapping), seeded via sml-prng. This
   pins determinism: same seed -> same table -> same noise, on both compilers. *)

structure Noise :> NOISE =
struct
  structure V2 = Glm.Vec2
  structure R = Prng.SplitMix64

  type t = { perm : int vector }   (* length 512 *)

  fun fromSeed seed =
    let
      val base = List.tabulate (256, fn i => i)
      val (shuffled, _) = R.shuffle base (R.seed seed)
      val arr = Vector.fromList (shuffled @ shuffled)   (* doubled to 512 *)
    in
      { perm = arr }
    end

  fun perm ({perm} : t) i = Vector.sub (perm, Int.mod (i, 512))
  (* mask an integer lattice coordinate into [0,255] without negative mod *)
  fun wrap255 i = Int.mod (Int.mod (i, 256) + 256, 256)

  fun fade t = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
  fun lerp (a, b, t) = a + t * (b - a)
  fun ffloor x = Real.realFloor x
  fun ifloor x = Real.floor x

  (* ---- gradients ---- *)
  (* 2D: 8 directions on the unit-ish circle (Perlin's improved set). *)
  fun grad2 (h, x, y) =
    case Int.mod (h, 8) of
        0 => x + y
      | 1 => x - y
      | 2 => ~x + y
      | 3 => ~x - y
      | 4 => x
      | 5 => ~x
      | 6 => y
      | _ => ~y

  (* 3D: Perlin's 12 edge gradients. *)
  fun grad3 (h, x, y, z) =
    case Int.mod (h, 12) of
        0 => x + y    | 1 => ~x + y   | 2 => x - y    | 3 => ~x - y
      | 4 => x + z    | 5 => ~x + z   | 6 => x - z    | 7 => ~x - z
      | 8 => y + z    | 9 => ~y + z   | 10 => y - z   | _ => ~y - z

  fun perlin2 (ctx : t) (x, y) =
    let
      val xi = wrap255 (ifloor x)
      val yi = wrap255 (ifloor y)
      val xf = x - ffloor x
      val yf = y - ffloor y
      val u = fade xf
      val v = fade yf
      val p = perm ctx
      val aa = p (p xi + yi)
      val ab = p (p xi + yi + 1)
      val ba = p (p (xi + 1) + yi)
      val bb = p (p (xi + 1) + yi + 1)
      val x1 = lerp (grad2 (aa, xf, yf), grad2 (ba, xf - 1.0, yf), u)
      val x2 = lerp (grad2 (ab, xf, yf - 1.0), grad2 (bb, xf - 1.0, yf - 1.0), u)
    in
      lerp (x1, x2, v)
    end

  fun perlin3 (ctx : t) (x, y, z) =
    let
      val xi = wrap255 (ifloor x)
      val yi = wrap255 (ifloor y)
      val zi = wrap255 (ifloor z)
      val xf = x - ffloor x
      val yf = y - ffloor y
      val zf = z - ffloor z
      val u = fade xf
      val v = fade yf
      val w = fade zf
      val p = perm ctx
      val aaa = p (p (p xi + yi) + zi)
      val aba = p (p (p xi + yi + 1) + zi)
      val aab = p (p (p xi + yi) + zi + 1)
      val abb = p (p (p xi + yi + 1) + zi + 1)
      val baa = p (p (p (xi+1) + yi) + zi)
      val bba = p (p (p (xi+1) + yi + 1) + zi)
      val bab = p (p (p (xi+1) + yi) + zi + 1)
      val bbb = p (p (p (xi+1) + yi + 1) + zi + 1)
      val x1 = lerp (grad3 (aaa, xf, yf, zf), grad3 (baa, xf-1.0, yf, zf), u)
      val x2 = lerp (grad3 (aba, xf, yf-1.0, zf), grad3 (bba, xf-1.0, yf-1.0, zf), u)
      val y1 = lerp (x1, x2, v)
      val x3 = lerp (grad3 (aab, xf, yf, zf-1.0), grad3 (bab, xf-1.0, yf, zf-1.0), u)
      val x4 = lerp (grad3 (abb, xf, yf-1.0, zf-1.0), grad3 (bbb, xf-1.0, yf-1.0, zf-1.0), u)
      val y2 = lerp (x3, x4, v)
    in
      lerp (y1, y2, w)
    end

  (* ---- value noise (smoothstep over lattice values in [0,1]) ---- *)
  fun latticeVal (ctx : t) (i, j) =
    Real.fromInt (perm ctx (perm ctx (wrap255 i) + wrap255 j)) / 255.0
  fun lattice3Val (ctx : t) (i, j, k) =
    Real.fromInt
      (perm ctx (perm ctx (perm ctx (wrap255 i) + wrap255 j) + wrap255 k)) / 255.0

  fun value2 (ctx : t) (x, y) =
    let
      val xi = ifloor x and yi = ifloor y
      val xf = x - ffloor x and yf = y - ffloor y
      val u = fade xf and v = fade yf
      val v00 = latticeVal ctx (xi, yi)
      val v10 = latticeVal ctx (xi+1, yi)
      val v01 = latticeVal ctx (xi, yi+1)
      val v11 = latticeVal ctx (xi+1, yi+1)
    in
      lerp (lerp (v00, v10, u), lerp (v01, v11, u), v)
    end

  fun value3 (ctx : t) (x, y, z) =
    let
      val xi = ifloor x and yi = ifloor y and zi = ifloor z
      val xf = x - ffloor x and yf = y - ffloor y and zf = z - ffloor z
      val u = fade xf and v = fade yf and w = fade zf
      fun lv (i,j,k) = lattice3Val ctx (i,j,k)
      val c000 = lv (xi,yi,zi)     val c100 = lv (xi+1,yi,zi)
      val c010 = lv (xi,yi+1,zi)   val c110 = lv (xi+1,yi+1,zi)
      val c001 = lv (xi,yi,zi+1)   val c101 = lv (xi+1,yi,zi+1)
      val c011 = lv (xi,yi+1,zi+1) val c111 = lv (xi+1,yi+1,zi+1)
      val x00 = lerp (c000, c100, u)  val x10 = lerp (c010, c110, u)
      val x01 = lerp (c001, c101, u)  val x11 = lerp (c011, c111, u)
      val y0 = lerp (x00, x10, v)     val y1 = lerp (x01, x11, v)
    in
      lerp (y0, y1, w)
    end

  (* ---- simplex 2D/3D (Perlin's simplex, gradient sum) ---- *)
  val f2 = 0.5 * (Math.sqrt 3.0 - 1.0)
  val g2 = (3.0 - Math.sqrt 3.0) / 6.0

  fun simplex2 (ctx : t) (xin, yin) =
    let
      val s = (xin + yin) * f2
      val i = ifloor (xin + s)
      val j = ifloor (yin + s)
      val tt = Real.fromInt (i + j) * g2
      val x0 = xin - (Real.fromInt i - tt)
      val y0 = yin - (Real.fromInt j - tt)
      val (i1, j1) = if x0 > y0 then (1, 0) else (0, 1)
      val x1 = x0 - Real.fromInt i1 + g2
      val y1 = y0 - Real.fromInt j1 + g2
      val x2 = x0 - 1.0 + 2.0 * g2
      val y2 = y0 - 1.0 + 2.0 * g2
      val ii = wrap255 i and jj = wrap255 j
      val p = perm ctx
      val gi0 = p (ii + p jj)
      val gi1 = p (ii + i1 + p (jj + j1))
      val gi2 = p (ii + 1 + p (jj + 1))
      fun corner (x, y, gi) =
        let val tcoef = 0.5 - x*x - y*y
        in if tcoef < 0.0 then 0.0
           else let val t4 = tcoef*tcoef*tcoef*tcoef
                in t4 * grad2 (gi, x, y) end
        end
    in
      70.0 * (corner (x0,y0,gi0) + corner (x1,y1,gi1) + corner (x2,y2,gi2))
    end

  val f3 = 1.0 / 3.0
  val g3 = 1.0 / 6.0

  fun simplex3 (ctx : t) (xin, yin, zin) =
    let
      val s = (xin + yin + zin) * f3
      val i = ifloor (xin + s) and j = ifloor (yin + s) and k = ifloor (zin + s)
      val tt = Real.fromInt (i + j + k) * g3
      val x0 = xin - (Real.fromInt i - tt)
      val y0 = yin - (Real.fromInt j - tt)
      val z0 = zin - (Real.fromInt k - tt)
      val (i1,j1,k1, i2,j2,k2) =
        if x0 >= y0 then
          if y0 >= z0 then (1,0,0, 1,1,0)
          else if x0 >= z0 then (1,0,0, 1,0,1)
          else (0,0,1, 1,0,1)
        else
          if y0 < z0 then (0,0,1, 0,1,1)
          else if x0 < z0 then (0,1,0, 0,1,1)
          else (0,1,0, 1,1,0)
      val x1 = x0 - Real.fromInt i1 + g3
      val y1 = y0 - Real.fromInt j1 + g3
      val z1 = z0 - Real.fromInt k1 + g3
      val x2 = x0 - Real.fromInt i2 + 2.0*g3
      val y2 = y0 - Real.fromInt j2 + 2.0*g3
      val z2 = z0 - Real.fromInt k2 + 2.0*g3
      val x3 = x0 - 1.0 + 3.0*g3
      val y3 = y0 - 1.0 + 3.0*g3
      val z3 = z0 - 1.0 + 3.0*g3
      val ii = wrap255 i and jj = wrap255 j and kk = wrap255 k
      val p = perm ctx
      val gi0 = p (ii + p (jj + p kk))
      val gi1 = p (ii + i1 + p (jj + j1 + p (kk + k1)))
      val gi2 = p (ii + i2 + p (jj + j2 + p (kk + k2)))
      val gi3 = p (ii + 1 + p (jj + 1 + p (kk + 1)))
      fun corner (x, y, z, gi) =
        let val tcoef = 0.6 - x*x - y*y - z*z
        in if tcoef < 0.0 then 0.0
           else let val t4 = tcoef*tcoef*tcoef*tcoef
                in t4 * grad3 (gi, x, y, z) end
        end
    in
      32.0 * (corner (x0,y0,z0,gi0) + corner (x1,y1,z1,gi1)
              + corner (x2,y2,z2,gi2) + corner (x3,y3,z3,gi3))
    end

  (* ---- Worley / cellular (F1, F2), one feature point per cell ---- *)
  fun featurePoint (ctx : t) (cx, cy) =
    let
      val p = perm ctx
      (* two hashed values in [0,1) as the in-cell jitter *)
      val hx = p (wrap255 cx + p (wrap255 cy))
      val hy = p (wrap255 cx + 1 + p (wrap255 cy + 1))
      val fx = Real.fromInt hx / 256.0
      val fy = Real.fromInt hy / 256.0
    in
      (Real.fromInt cx + fx, Real.fromInt cy + fy)
    end

  fun worley2 (ctx : t) (x, y) =
    let
      val xi = ifloor x and yi = ifloor y
      val best = ref (1.0E30, 1.0E30)
      fun consider (cx, cy) =
        let
          val (fx, fy) = featurePoint ctx (cx, cy)
          val d = V2.dist (V2.v (fx, fy), V2.v (x, y))
          val (f1, f2) = !best
        in
          if d < f1 then best := (d, f1)
          else if d < f2 then best := (f1, d)
          else ()
        end
      fun loopX i =
        if i > 1 then ()
        else (loopY (i, ~1); loopX (i+1))
      and loopY (i, jj) =
        if jj > 1 then ()
        else (consider (xi + i, yi + jj); loopY (i, jj+1))
    in
      loopX (~1);
      !best
    end

  (* ---- fbm / turbulence ---- *)
  type fbmParams = { octaves : int, lacunarity : real, gain : real }

  fun accumulate transform ({octaves, lacunarity, gain} : fbmParams) basis (x, y) =
    let
      fun loop (0, _, _, acc) = acc
        | loop (n, freq, amp, acc) =
            let val s = transform (basis (x * freq, y * freq))
            in loop (n - 1, freq * lacunarity, amp * gain, acc + amp * s) end
    in
      if octaves <= 0 then 0.0
      else loop (octaves, 1.0, 1.0, 0.0)
    end

  fun fbm2 params basis xy = accumulate (fn s => s) params basis xy
  fun turbulence2 params basis xy = accumulate Real.abs params basis xy
end
