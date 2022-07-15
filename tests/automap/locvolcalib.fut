-- ==
-- entry: tridagPar
-- compiled random input { [10]f32 [10]f32 [10]f32 [10]f32 }
-- output { true }

def tridagPar_orig [n] (a:  [n]f32, b: [n]f32, c: [n]f32, y: [n]f32 ): *[n]f32 =
  let b0   = b[0]
  let mats = map  (\i ->
                     if 0 < i
                     then (b[i], 0.0-a[i]*c[i-1], 1.0, 0.0)
                     else (1.0,  0.0,             0.0, 1.0))
                  (iota n)
  let scmt = scan (\(a0,a1,a2,a3) (b0,b1,b2,b3) ->
                     let value = 1.0/(a0*b0)
                     in ( (b0*a0 + b1*a2)*value,
                          (b0*a1 + b1*a3)*value,
                          (b2*a0 + b3*a2)*value,
                          (b2*a1 + b3*a3)*value))
                  (1.0,  0.0, 0.0, 1.0) mats
  let b    = map (\(t0,t1,t2,t3) -> (t0*b0 + t1) / (t2*b0 + t3)) scmt
  let y0   = y[0]
  let lfuns= map  (\i  ->
                     if 0 < i
                     then (y[i], 0.0-a[i]/b[i-1])
                     else (0.0,  1.0))
                  (iota n)
  let cfuns= scan (\(a0,a1) (b0,b1) -> (b0 + b1*a0, a1*b1))
                  (0.0, 1.0) lfuns
  let y    = map (\(a,b)  -> a + b*y0) cfuns
  let yn   = y[n-1]/b[n-1]
  let lfuns= map (\k  ->
                    let i = n-k-1
                    in  if   0 < k
                        then (y[i]/b[i], 0.0-c[i]/b[i])
                        else (0.0,       1.0))
                 (iota n)
  let cfuns= scan (\(a0,a1) (b0,b1) -> (b0 + b1*a0, a1*b1))
                  (0.0, 1.0) lfuns
  let y    = map (\(a,b) -> a + b*yn) cfuns
  let y    = reverse y
  in y


def tridagPar_am [n] (a:  [n]f32, b: [n]f32, c: [n]f32, y: [n]f32 ): *[n]f32 =
  let b0   = b[0]
  let mats = map  (\i ->
                     if 0 < i
                     then (b[i], 0.0-a[i]*c[i-1], 1.0, 0.0)
                     else (1.0,  0.0,             0.0, 1.0))
                  (iota n)
  let scmt = scan (\(a0,a1,a2,a3) (b0,b1,b2,b3) ->
                     let value = 1.0/(a0*b0)
                     in ( (b0*a0 + b1*a2)*value,
                          (b0*a1 + b1*a3)*value,
                          (b2*a0 + b3*a2)*value,
                          (b2*a1 + b3*a3)*value))
                  (1.0,  0.0, 0.0, 1.0) mats
  let b    = (scmt.0*b0 + scmt.1) / (scmt.2*b0+scmt.3)
  let y0   = y[0]
  let lfuns= map  (\i  ->
                     if 0 < i
                     then (y[i], 0.0-a[i]/b[i-1])
                     else (0.0,  1.0))
                  (iota n)
  let cfuns= scan (\(a0,a1) (b0,b1) -> (b0 + b1*a0, a1*b1))
                  (0.0, 1.0) lfuns
  let y    = cfuns.0 + cfuns.1 * y0
  let yn   = y[n-1]/b[n-1]
  let lfuns= map (\k  ->
                    let i = n-k-1
                    in  if   0 < k
                        then (y[i]/b[i], 0.0-c[i]/b[i])
                        else (0.0,       1.0))
                 (iota n)
  let cfuns = scan (\(a0,a1) (b0,b1) -> (b0 + b1*a0, a1*b1))
                   (0.0, 1.0) lfuns
  in reverse (cfuns.0 + cfuns.1 * yn)

entry tridagPar [n] (a:  [n]f32, b: [n]f32, c: [n]f32, y: [n]f32 ): bool =
  tridagPar_orig a b c y == tridagPar_am a b c y 
