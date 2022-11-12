type node_ratio = Inf | Ratio of ((int * int) * (int * int)) * (ratio_num*ratio_num)
and ratio_num = unit -> node_ratio

let rec split_ratio ((a,b) as q1,(c,d as q2) as q) () = let h = (a+c,b+d)  in Ratio(q,(split_ratio (q1,h),split_ratio (h,q2)))

let rationals = split_ratio ((0,1),(1,0))
