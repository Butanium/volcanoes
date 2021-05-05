(*TODO fix double objective not working *)
module IntSet = Set.Make(Int);;
let (-@) a b  = fun x -> a (b x) in
let opp s = (if s.[0] = 'N' then "S" else "N") ^ String.sub s 1 2 in
(* Win by building an unbroken chain of volcanoes from one side of the board to the other. *)
let numberoftiles = int_of_string (input_line stdin) in (* Number of tiles on the board *)

let scoreTable = Hashtbl.create(numberoftiles) and
    indexTable = Hashtbl.create(numberoftiles) and
    nameTable = Hashtbl.create(numberoftiles) and
    neighborsTable = Hashtbl.create(numberoftiles) in
let getScore i = Hashtbl.find scoreTable i and setScore i s = Hashtbl.replace scoreTable i s and
    toIndex s = Hashtbl.find indexTable s and
    toName i = Hashtbl.find nameTable i and
    getNeighbors i = Hashtbl.find neighborsTable i in
for i = 0 to numberoftiles - 1 do
    (* tilename: Name of the tile at this index (e.g., N01 or S25) *)
    (* neighbor1: Index of a neighboring tile *)
    (* neighbor2: Index of a neighboring tile *)
    (* neighbor3: Index of a neighboring tile *)

    let tilename, neighbor1, neighbor2, neighbor3 = Scanf.sscanf (input_line stdin) " %s  %d  %d  %d" (fun tilename neighbor1 neighbor2 neighbor3 -> (tilename, neighbor1, neighbor2, neighbor3)) in
    Hashtbl.add scoreTable i 0.;
    Hashtbl.add indexTable tilename i;
    Hashtbl.add nameTable i tilename;
    Hashtbl.add neighborsTable i [neighbor1; neighbor2 ; neighbor3]
done;
let oppTable = let r = Hashtbl.create numberoftiles in
    for i = 0 to numberoftiles - 1 do
        Hashtbl.add r i @@ toIndex @@ opp @@ toName i;
    done;
    r in
let oppIndex i = try Hashtbl.find oppTable i with Not_found -> failwith (Printf.sprintf "couldn't find this index : %d" i) in



(* game loop *)
let getDoubleAndSimple g =
    let rec aux accDouble double simple = function
        | [] -> double, simple
        | x :: xs -> let oppX = oppIndex x in (if IntSet.mem x accDouble then aux accDouble double simple else
            if List.mem oppX xs then
                aux (IntSet.add oppX accDouble) ((x, oppX) :: double) simple
            else aux accDouble double @@ oppX :: simple) xs
    in aux IntSet.empty [] [] g and
    getMyPositions pos =
        let rec aux acc i = function
            | [] -> acc
            | x :: xs -> (if x > 0 then aux (i :: acc) (i+1) else aux acc (i+1)) xs
        in aux [] 0 pos in


let startScore = 100. and doubleRatio = 10. in
let basicQueue = Queue.create() and
    minQueue = ref startScore and
    priorityQueue = Queue.create() in
let addGains x = let i, amount = x in
    minQueue := min !minQueue amount;
    if (getScore i) < amount then (
        setScore i amount;
        let queue = if amount/. 2. > !minQueue then priorityQueue else basicQueue in
        List.iter (fun i -> Queue.add (i,amount /. 2.) queue) @@ getNeighbors i
    ) in


let getBool : bool * 'a -> bool = function
    | b, _ -> b in

let positionArr = Array.make numberoftiles 0. in

let isNearHashQueue x visitHash queue =
    let pos, depth = x in
    let rec aux = function
    | [] -> false
    | x :: xs -> if not @@ Hashtbl.mem visitHash x then positionArr.(x)>0. ||
                (Queue.add (x, depth + 1) queue; Hashtbl.add visitHash x 0 ; aux xs)
                 else aux xs
    in aux @@ getNeighbors pos in





let isNearAllyQueue x queue = let pos, toIgnore, depth = x in
     let neighbors = List.filter (fun x -> not @@ List.mem x toIgnore) @@ getNeighbors pos in
     let toIgnore = neighbors @ toIgnore in
     let rec aux = function
        | x :: xs -> positionArr.(x) > 0. || (Queue.add (x, toIgnore, depth + 1) queue; aux xs)
        | [] -> false
     in aux neighbors in

let isNearAllyLimit tile depth =
    let queue = Queue.create() and table = Hashtbl.create @@ numberoftiles/2 in
    Queue.add (tile, 0) queue;
    Hashtbl.add table tile 0;
    let rec aux() =
        if Queue.is_empty queue then false else (
            let _,len as params = Queue.take queue in
            len < depth && (isNearHashQueue params table queue || aux())
        )
    in aux() in


let countAllies x = List.fold_left (+.) 0. @@ List.map (fun t -> max 0. positionArr.(t)) @@ getNeighbors x in
let calcScore x = getScore x -. positionArr.(x) -. countAllies x in

while true do
    let t = Sys.time() in

    let position = List.map int_of_string @@ String.split_on_char ' ' @@ input_line stdin in
    List.iteri (fun i x -> positionArr.(i) <- float_of_int x) position;
    (* Space-separated list of the volcano levels for every tile on the board (in index order); value will be positive for your volcanoes, negative for your opponent's volcanoes, or 0 if empty *)
    let moves = String.split_on_char ' ' @@ input_line stdin in (* Space-separate list of all valid moves in this position *)
    let myPositions = getMyPositions position in
    let intMoves = List.map toIndex moves and
        doubleGoals, simpleGoals = getDoubleAndSimple @@ myPositions in

    let filteredMoves = List.filter (fun tile -> isNearAllyLimit tile 3) intMoves in
    let filteredMoves2 = List.filter (fun tile -> isNearAllyLimit tile 2) intMoves in
    let rec aux = function
        | a :: ass , b::bs -> prerr_endline @@ Printf.sprintf "%s, %s" (toName a) (toName b); aux (ass,bs)
        | a :: ass, [] -> prerr_endline @@ toName a ^ ", NULL"; aux (ass,[])
        | [], b :: bs -> prerr_endline @@ "NULL, " ^ toName b; aux ([],bs)
        | _ -> () in
        aux (filteredMoves,filteredMoves2);


    let it couple =
        let a,b = couple and searchQueue1, searchQueue2 = Queue.create(), Queue.create()
        and table1, table2 = Hashtbl.create (numberoftiles/2), Hashtbl.create (numberoftiles/2) in
        Queue.add (a, 0) searchQueue1;
        Queue.add (b, 0) searchQueue2;
        Hashtbl.add table1 a 0;
        Hashtbl.add table2 b 0;
        let add2Gains fst snd =  addGains (fst, startScore *. doubleRatio);
            addGains (snd, startScore); in
        let checks table queue fst snd minLen =
            let _,len as params = Queue.take queue in
            if len > minLen then (
                add2Gains fst snd;
                true
            ) else (
                if isNearHashQueue params table queue then (
                    addGains (fst, startScore *. doubleRatio);
                    addGains (snd, startScore *. doubleRatio);
                    true
                ) else false
            ) in
        let rec aux isNear1 isNear2 minLen =
            if isNear1 && isNear2 then (
                addGains (a, startScore *. doubleRatio);
                addGains (b, startScore *. doubleRatio);
            )
            else if isNear1 then
                if not @@ checks table2 searchQueue2 a b minLen then
                    aux isNear1 isNear2 minLen
            else if isNear2 then
                if not @@ checks table1 searchQueue1 b a minLen then
                    aux isNear1 isNear2 minLen
            else if Queue.is_empty searchQueue1 then add2Gains a b
            else if Queue.is_empty searchQueue2 then add2Gains b a
            else (
                let params1, params2 = Queue.take searchQueue1, Queue.take searchQueue2 in
                let (_, len1), (_, len2) = params1, params2 in
                let t1,t2 = isNearHashQueue params1 table1 searchQueue1, isNearHashQueue params2 table2 searchQueue2 in
                aux (t1 && not @@ t2 && len2<len1) (t2 && not @@ t1 && len1<len2)
                (if t1 == t2 then min len1 len2 else if t1 then len1 else len2)
            ) in aux false false 0 in

    List.iter it doubleGoals;
    List.iter (fun i -> addGains (i, startScore)) simpleGoals;

    while Sys.time() -. t < 0.09 && not (Queue.is_empty basicQueue && Queue.is_empty priorityQueue) do
        if not (Queue.is_empty priorityQueue) then
            addGains (Queue.take priorityQueue)
        else
            addGains (Queue.take basicQueue);
    done;
    let sortedIndex = List.sort (fun x y -> -compare (calcScore x) (calcScore y)) @@ List.mapi (fun i _ -> i) position in
    List.iter (fun tile  -> if (getScore tile > 0.) then prerr_endline @@ Printf.sprintf "%s : %f" (toName tile) (getScore tile)) @@ sortedIndex;
    prerr_endline "____ sorted _____";
    let sorted = List.sort (fun x y -> -compare (calcScore x) (calcScore y)) filteredMoves in
    List.iter (fun i -> prerr_endline @@ Printf.sprintf "%s : %f" (toName i) (calcScore i)) sorted;
    match sorted with
    | [] -> print_endline "random";
    | x :: xs -> print_endline (toName x);
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)
    (* Either RANDOM or a tile name (e.g., N12 or S34) *)

done;;
