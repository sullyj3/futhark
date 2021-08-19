-- Memory block merging with a chain that uses all three coalescing-enabled
-- constructs.
-- ==
-- input { [7, 0, 7] }
-- output { [[0, 0, 0, 0, 0, 0], [7, 0, 7, 8, 1, 8]] }
-- structure cpu { Alloc 1 }
-- structure gpu { Alloc 1 }

let main [n] (ns: [n]i32): [][]i32 =
  -- Will initially be set to use the memory of t1.  Will end up using the
  -- memory of t3 through t2 through t1.
  let t0 = map (+ 1) ns

  -- Will use the second part of index 1 of the memory of t3 through the memory
  -- of t2.
  let t1 = copy t0

  -- Will use index 1 of the memory of t3.
  let t2 = concat_to (n * 2) ns t1

  -- Will be the only remaining memory block.
  let t3 = replicate 2 (replicate (n * 2) 0)
  let t3[1] = t2

  in t3
