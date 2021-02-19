open Test_lib
open Report

(*TYPE DEFINITIONS
type 'a tree = Node of 'a * ('a tree) list*)

(*EXPRESSION DEFINITIONS*)
let l5 = [0; 1; 2; 3; 4; 5]
let l10 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let tLeaf = Node (1, [])
let tDeep = Node (1, [ Node (2, [ Node (3, [ Node (4, [Node (5, [])])])])])
let tWide = Node (1, [ Node (2, [])
                     ; Node (3, [])
                     ; Node (4, [])
                     ; Node (5, [])
                     ])
let t10 = Node (1, [ Node (2, [])
                   ; Node (3, [ Node (4, [])
                              ; Node (5, [])
                              ; Node (6, [ Node (7, [])
                                         ; Node (8, [])
                                         ; Node (9, [])
                                         ; Node (10, [])
                                         ])
                              ])
                   ])

(*SAMPLER DEFINITIONS*)
exception InvalidSampler of string

(* Palindrome sampler
ratio = percent chance of getting a palindrome element. Please give a ratio between 0 and 1.
Note: set ratio higher than desired chance of getting palindrome. Chance of getting palindrome = ratio^(floor(size/2)).
This means that the longer the list the less likely a palindrom will be generated. 
This is sampled in this way since otherwise we would have to use log function.*)
let sample_palindrome ?(max_halfSize = 4) ?(ratio = 0.75) (sample_el : 'a sampler) : 'a list sampler = fun () ->
  if ratio < 0. || ratio > 1. then raise (InvalidSampler "Palindrome_Sampler: Ratio must be between 0 and 1!");
  let l1 = sample_list ~max_size:max_halfSize sample_el () in
  let l2 = List.map (fun x -> 
    if Random.float 1.0 < ratio then x
    else sample_el ()
  ) l1 in
  let l2 = if Random.bool () then (sample_el ()) :: l2 else l2 in
  List.rev_append l1 l2

(* Squeeze Sampler 
ratio = chance of producing consecutive elements*)
let sample_squeeze_list ?(max_size = 10) ?(ratio = 0.5) (sample_el : 'a sampler) : 'a list sampler = fun () ->
  if ratio < 0. || ratio > 1. then raise (InvalidSampler "Palindrome_Sampler: Ratio must be between 0 and 1!");
  let rec gen_list n acc =
    if n = 0 then acc else 
    let acc = match acc with
      | [] -> sample_el () :: acc
      | x :: xs when Random.float 1.0 < ratio -> x :: acc
      | _ -> sample_el () :: acc
    in gen_list (n - 1) acc
  in
  gen_list (Random.int max_size) []


let optArg_rand_int min max = function
  | None -> Random.int (max - min + 1) + min
  | Some x -> x

(* Tree sampler *)
let sample_tree ?(max_depth = Random.int 10) ?(max_branch_factor = Random.int 10 + 1) (sample_el : 'a sampler) : 'a tree sampler = fun () ->
  if max_branch_factor <= 0 then raise (InvalidSampler "Tree_Sampler: max_branch_factor must be greater than 0!");
  
  let leaf = fun () -> Node(sample_el (), []) in 

  let rec node_sampler cur_depth =
    if cur_depth = max_depth then 
      leaf ()
    else
      let b_factor = (Random.int max_branch_factor) in
      let children = children_sampler cur_depth b_factor in
      Node(
        sample_el (), 
        children
      )

  and children_sampler cur_depth b_factor =
    let rec buildChildren n acc = 
      if n = 0 then acc else
      let child =
        (* minimize size of trees a little to encourage smaller trees *)
        if Random.int max_depth <= cur_depth ||           (* generates more leafs the deeper we go *)  
            Random.int max_branch_factor < b_factor then  (* generates more leafs the more children a node has *)
          leaf ()
        else
          node_sampler (cur_depth + 1)
      in buildChildren (n - 1) (child :: acc)
    in
    buildChildren b_factor []
  
  in
  node_sampler 0



(* 
(*let rec sample_tree ((sample: unit -> 'a), (maxHeightandWidth: int)): unit -> 'a tree = 
  fun () ->*)
let rec sample_tree (sample: unit -> 'a) (maxHeightandWidth: int) ():'a tree = 
  let rec buildChildren n = 
    if n <= 0 then 
      []
    else 
      (sample_tree sample (maxHeightandWidth-1) ())::(buildChildren (n-1))
  in
  begin
    if maxHeightandWidth <= 1 then 
      Node(sample(), [])
    else  
      
      let tree_sampler = sample_tree sample (maxHeightandWidth-1) in
      let children = sample_list 
                      ~min_size: 0
                      ~max_size: maxHeightandWidth
                      ~dups: false
                      ~sorted: false
                      tree_sampler
      in
      
      let children = buildChildren (Random.int (maxHeightandWidth+1)) in
      Node(sample(), children)
  end
 *)


(*TESTING FUNCTIONS*)
let test_identity () = 
  begin
    test_function_1_against_solution
      [%ty: int -> int]
      "identity"
      ~gen:1
      []
    @
    test_function_1_against_solution
      [%ty: float -> float]
      "identity"
      ~gen:1
      []
    @
    test_function_1_against_solution
      [%ty: char -> char]
      "identity"
      ~gen:1
      []
    @
    test_function_1_against_solution
      [%ty: string -> string]
      "identity"
      ~gen:1
      []
    @
    test_function_1_against_solution
      [%ty: bool -> bool]
      "identity"
      ~gen:1
      []
  end


let test_reverse () = 
  begin
    test_function_1_against_solution
      [%ty: int list -> int list]
      "reverse"
      ~gen:8
      [[] ; [0]]
  end
  

let test_palindrome () = 
  begin 
    test_function_1_against_solution
      [%ty: int list -> bool] "palindrome"
      ~sampler: (sample_palindrome sample_int)
      ~gen:4
      [[] ; [0] ; [1;3;3;1]]
    @
    test_function_1_against_solution
    [%ty: string list -> bool] "palindrome"
      ~sampler: (sample_palindrome sample_string)
      ~gen:1
      [["a";"b"; "a"]; ["aa"; "ba"; "a"]]
  end 


let test_squeeze () = 
  (* sample_alternatively takes a list of samplers and returns a sampler that
      mimics one of the samplers in the list at random each call *)
  let sampler = sample_alternatively [
    sample_squeeze_list ~ratio:0.25 sample_int; (* small amount of consecutive elements *)
    sample_squeeze_list ~ratio:0.5 sample_int;  (* medium amount of consecutive elements *)
    sample_squeeze_list ~ratio:0.75 sample_int; (* large amount of consecutive elements *)
  ] in
  begin
    test_function_1_against_solution
      [%ty: int list  -> int list ]
      "squeeze"
      ~sampler: sampler
      ~gen:13
      [[] ; [0]]
  end



let test_sprinkle () = 
  begin 
    test_function_1_against_solution
      [%ty: int list -> (int list * int list) list]
      "sprinkle"
      ~gen:10
      [[] ; [0]]
    @
    test_function_1_against_solution
      [%ty: char list -> (char list * char list) list]
      "sprinkle"
      ~gen:5
      []
    @
    test_function_1_against_solution
      [%ty: string list -> (string list * string list) list]
      "sprinkle"
      ~gen:3
      []
  end 

(* 
let test_enumerateKCombinations () = 
  begin
    test_function_1_against_solution
      [%ty: (int list * int) -> int list list]
      "enumerateKCombinations"
      ~gen:0
      [(l5,0);(l5,1);(l5,2);(l5,3);(l5,4);
       (l5,5);(l5,6);(l10,-1);(l10,5);(l10,12)]
  end
 *)

let test_countNodes () = 
  let sampler = sample_alternatively [
    sample_tree ~max_depth:10 ~max_branch_factor:1 sample_int; (* Skinny *)
    sample_tree ~max_depth:3 ~max_branch_factor:15 sample_int; (* Wide *)
    sample_tree sample_int; (* Random *)
    sample_tree sample_int; (* Random *)
  ] in
  begin
    test_function_1_against_solution
      [%ty: int tree -> int] "countNodes"
      ~sampler:sampler
      ~gen:10
      []
  end

let test_ironOut () = 
  let sampler = sample_alternatively [
    sample_tree ~max_depth:10 ~max_branch_factor:1 sample_int; (* Skinny *)
    sample_tree ~max_depth:3 ~max_branch_factor:15 sample_int; (* Wide *)
    sample_tree sample_int; (* Random *)
    sample_tree sample_int; (* Random *)
  ] in
  begin
    test_function_1_against_solution
      [%ty: int tree -> int list list]
      "ironOut"
      ~sampler:sampler
      ~gen:20
      []
  end

let test_fib () = 
  begin
    test_function_1_against_solution
      [%ty: int -> int list]
      "fib"
      ~sampler: (fun () -> Random.int 10000)
      ~gen:10
      []
  end

let test_pascal () = 
  begin
    test_function_1_against_solution
      [%ty: int -> int list list]
      "pascal"
      ~sampler: (fun () -> Random.int 21)
      ~gen:5
      []
  end






(*AGGREGATED CALL FOR ALL TEST FUNCTIONS*)
let () = 
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [
    Section
      ([ Text "Function" ; Code "identity" ],
        test_identity());   
    Section
      ([ Text "Function" ; Code "reverse" ],
      test_reverse());
    Section 
      ([Text "Function"; Code "palindrome"],
        test_palindrome());  
      
    Section
      ([Text "Function"; Code  "squeeze"],
      test_squeeze());

    Section
      ([ Text "Function" ; Code "sprinkle" ],
      test_sprinkle());
(* 
    Section
      ([ Text "Function" ; Code "enumerateKCombinations" ],
      test_enumerateKCombinations());
 *)
    Section
      ([ Text "Function" ; Code "countNodes" ],
      test_countNodes());

    Section
      ([ Text "Function" ; Code "ironOut" ],
      test_ironOut());
      
    Section
      ([ Text "Function" ; Code "fib" ],
      test_fib());
    Section
      ([ Text "Function" ; Code "pascal" ],
      test_pascal())
    
  ]