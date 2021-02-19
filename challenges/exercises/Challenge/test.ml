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
      (*
      let tree_sampler = sample_tree sample (maxHeightandWidth-1) in
      let children = sample_list 
                      ~min_size: 0
                      ~max_size: maxHeightandWidth
                      ~dups: false
                      ~sorted: false
                      tree_sampler
      in
      *)
      let children = buildChildren (Random.int (maxHeightandWidth+1)) in
      Node(sample(), children)
  end
  
(*TESTING FUNCTIONS*)
let test_identity () = 
  begin
    test_function_1_against_solution
      [%ty: int -> int]
      "identity"
      ~gen:0
      [0]
    @
    test_function_1_against_solution
      [%ty: float -> float]
      "identity"
      ~gen:0
      [0.0]
    @
    test_function_1_against_solution
      [%ty: char -> char]
      "identity"
      ~gen:0
      ['a']
    @
    test_function_1_against_solution
      [%ty: string -> string]
      "identity"
      ~gen:0
      ["hello world"]
    @
    test_function_1_against_solution
      [%ty: bool -> bool]
      "identity"
      ~gen:0
      [true]
  end


  let test_reverse () = 
    begin
      test_function_1_against_solution
        [%ty: int list -> int list]
        "reverse"
        ~gen:10
        []
    end
  

let test_palindrome () = 
  begin 
    test_function_1_against_solution
      [%ty: int list -> bool]
      "palindrome"
       ~gen:0
       [[1;3;3;1]]
    @
    test_function_1_against_solution
      [%ty: int list -> bool]
      "palindrome"
       ~gen:0
       [[0]]
    @
    test_function_1_against_solution
    [%ty: string list -> bool]
    "palindrome"
      ~gen:4
      [["a"] ; ["a";"b"; "a"]; ["aa"; "ba"; "a"]]

    @
    test_function_1_against_solution
    [%ty : int list -> bool]
    "palindrome"
    ~gen:7
    []
  end 

  let sample_small () = Random.int 4 +1
let sample_big ()  = Random.int 10 +1
let test_squeeze () = 
  begin
    test_function_1_against_solution
      [%ty: int list  -> int list ]
      "squeeze"
      ~sampler:(fun () -> 
        begin  
          (sample_list ~max_size:10  ~dups: true  ~sorted: false sample_small) () 
        end)
      ~gen:10
      []
    @
    test_function_1_against_solution
      [%ty: int list  -> int list ]
      "squeeze"
      ~sampler: (fun () -> 
      begin  
        (sample_list ~max_size:10  ~dups: true  ~sorted: false sample_big) () 
      end)
      ~gen:10
      []
  end



let test_sprinkle () = 
  begin 
    test_function_1_against_solution
      [%ty: int list -> (int list * int list) list]
      "sprinkle"
      ~gen:10
      []
    @
    test_function_1_against_solution
      [%ty: char list -> (char list * char list) list]
      "sprinkle"
      ~gen:10
      []
    @
    test_function_1_against_solution
      [%ty: string list -> (string list * string list) list]
      "sprinkle"
      ~gen:10
      []
  end 


let test_enumerateKCombinations () = 
  begin
    test_function_1_against_solution
      [%ty: (int list * int) -> int list list]
      "enumerateKCombinations"
      ~gen:0
      [(l5,0);(l5,1);(l5,2);(l5,3);(l5,4);
       (l5,5);(l5,6);(l10,-1);(l10,5);(l10,12)]
  end

let test_countNodes () = 
  begin
    test_function_1_against_solution
      [%ty: int tree -> int]
      "countNodes"
      ~sampler:(sample_tree sample_int 5)
      ~gen:15
      []
  end

  let test_ironOut () = 
    begin
      test_function_1_against_solution
        [%ty: int tree -> int list list]
        "ironOut"
        ~sampler:(sample_tree sample_int 5)
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

    Section
      ([ Text "Function" ; Code "enumerateKCombinations" ],
      test_enumerateKCombinations());

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