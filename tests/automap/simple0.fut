-- ==
-- entry: main
-- compiled input { [1,2,3] [4,5,6] }
-- output { [5,7,9] }

entry main (xs: []i32) (ys: []i32) = (\x y -> x + y * 2) xs ys
