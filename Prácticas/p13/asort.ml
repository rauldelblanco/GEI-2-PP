let merge f a left mid right = 
    let temp = Array.make (Array.length a) (a.(0)) in 
        let p1,p2,k = ref left, ref (mid+1), ref left in
            while ((!p1 <= mid) && (!p2 <=right)) do
                if (f a.(!p1) a.(!p2))
                then (temp.(!k) <- a.(!p1); k := !k + 1; p1 := !p1 + 1)
                else (temp.(!k) <- a.(!p2); k := !k + 1; p2 := !p2 + 1)
            done;
            while (!p1 <= mid) do
               temp.(!k) <- a.(!p1); k:= !k +1; p1 := !p1 + 1
            done;
            while (!p2 <= right) do
               temp.(!k) <- a.(!p2); k:= !k +1; p2 := !p2 + 1
            done;
            for i = left to right do
                a.(i) <- temp.(i)
            done;;
            
let rec mergesort f a left right = 
    if (left < right)
    then let mid = (left + right) / 2 in
         mergesort f a left mid;
         mergesort f a (mid + 1) right;
         merge f a left mid right;;
         
let asort f a = mergesort f a 0 (Array.length a-1);;
