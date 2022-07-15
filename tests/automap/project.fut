-- ==
-- entry: main
-- compiled input { [1,2,3,4] [1,2,3,4] }
-- output { [2,4,6,8] }

entry main [n] (xs : [n]i32) (ys: [n]i32) : [n]i32 =
  let xys = zip xs ys
  in xys.0 + xys.1
  
