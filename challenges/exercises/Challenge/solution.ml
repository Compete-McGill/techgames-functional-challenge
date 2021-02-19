let solution _ = "This is the solution"

let identity x = x
;;


let rec reverse (l : 'a list) : 'a list = List.rev l
;;


let palindrome (l: 'a list) : bool   =
	l = List.rev l
;;


let rec squeeze l  = 
  match l with 
  | a :: (b :: _ as t) -> if a = b then squeeze t else a :: squeeze t
  | smaller -> smaller
;;


let rec sprinkle l =
	match l with 
	|[] -> [([],[])]
  |h :: tl -> let l2  = sprinkle tl in
      let rec left item l = 
        match  l with 
        | [] -> []
        | (p1,p2) :: rest -> (item ::p1, p2) :: (left item rest) in
      let rec right item  l  = 
        match l with
        |[] -> []
        |(p1,p2) :: rest -> (p1, item :: p2) :: (right item rest) in
      
      left  h l2 @ right h l2 
;;

(* 
let rec enumerateKCombinations ((l : 'a list), (k : int)) : 'a list list =
	if k <= 0 then [[]]
	else 
		begin
			match l with
		     | [] -> []
		     | h :: t ->
		        let with_h = List.map (fun l -> h :: l) (enumerateKCombinations (t, k-1)) in
		        let without_h = enumerateKCombinations (t, k) in
		        with_h @ without_h
		end
;;
 *)

let rec countNodes (t : 'a tree) : int = 
	let Node (value, children) = t in
	match children with
		| [] -> 1
		| _ -> List.fold_left (fun acc t' -> acc + countNodes t') 1 children
;;


let rec ironOut (t: int tree) : int list list = 
	let Node(x,ts) =  t in 
		match ts with
		| [] -> [ [ x ] ]
		| ts -> 
				List.map (fun xs -> x :: xs) @@ 
				List.concat @@ 
				List.map ironOut ts
;;


let fib max = 
	unfold 
		(fun (a,b) -> a, (b, a+b)) 
		(fun (a,b) -> a > max) 
		(1,1)
;;

let pascal max = 
	unfold
		(fun r -> 
			let next = List.map2 (+) ([0] @ r) (r @ [0]) in
			(r, next))
		(fun l -> List.length l >= max)
		[1]
;;




		