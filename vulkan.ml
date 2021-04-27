let opp s = (if s.[0] = 'N' then "S" else "N") ^ String.sub s 1 2 in
(* Win by building an unbroken chain of volcanoes from one side of the board to the other. *)
(* TODO: Add neighbour existence condition *)

let numberoftiles = int_of_string (input_line stdin) in (* Number of tiles on the board *)

let scoreTable = Hashtbl.create(numberoftiles) and
    indexTable = Hashtbl.create(numberoftiles) and
    nameTable = Hashtbl.create(numberoftiles) and
    neighborsTable = Hashtbl.create(numberoftiles) in
let getScore i = Hashtbl.find scoreTable i and setScore i s = Hashtbl.replace scoreTable i s and
    toIndex s = Hashtbl.find indexTable s and
    toName s = Hashtbl.find nameTable s and
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
let oppTable = let r = Hashtbl.create 80 in
    for i = 0 to numberoftiles - 1 do
        Hashtbl.add r i @@ toIndex @@ opp @@ toName i;
    done;
    r (*and
    mapHashKeys t f = let r = Hashtbl.create @@ Hashtbl.length t in
        Hashtbl.iter (fun k e -> Hashtbl.add r (f k) e) t;
        r*) in
let oppIndex i = try Hashtbl.find oppTable i with Not_found -> failwith (Printf.sprintf "couldn't find this index : %d" i) in



(* game loop *)
let getDoubleAndSimple g =
    let rec aux accDouble double simple = function
        | [] -> double, simple
        | x :: xs -> try (let oppX = oppIndex x in (if List.mem x accDouble then aux accDouble double simple else
            if List.mem oppX xs then
                aux (oppX :: accDouble) ((x, oppX) :: double) simple
            else aux accDouble double @@ x:: simple) xs) with none -> failwith "double simple fault"
    in aux [] [] [] g and
    getMyPositions pos =
        let rec aux acc i = function
            | [] -> acc
            | x :: xs -> (if x > 0 then aux (i :: acc) (i+1) else aux acc (i+1)) xs
        in aux [] 0 pos in


let startScore = 100. in
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

let positionArr = Array.make numberoftiles 0. in
let calcScore x = getScore x -. positionArr.(x) in

while true do
    let t = Sys.time() in

    let position = List.map int_of_string @@ String.split_on_char ' ' @@ input_line stdin in
    List.iteri (fun i x -> positionArr.(i) <- float_of_int x) position;
    (* Space-separated list of the volcano levels for every tile on the board (in index order); value will be positive for your volcanoes, negative for your opponent's volcanoes, or 0 if empty *)
    let moves = String.split_on_char ' ' @@ input_line stdin in (* Space-separate list of all valid moves in this position *)
    let myPositions = getMyPositions position in
    let intMoves = List.map toIndex moves and
        doubleGoals, simpleGoals = getDoubleAndSimple @@ myPositions in


    List.iter (fun i -> addGains (i, 100.)) @@
        List.map (fun x -> oppIndex x) myPositions;

    while Sys.time() -. t < 0.09 && not (Queue.is_empty basicQueue && Queue.is_empty priorityQueue) do
        if not (Queue.is_empty priorityQueue) then
            addGains (Queue.take priorityQueue)
        else
            addGains (Queue.take basicQueue);
    done;

    let sorted = List.sort (fun x y -> -compare (calcScore x) (calcScore y)) intMoves in
    match sorted with
    | [] -> print_endline "random";
    | x :: xs -> print_endline (toName x);
    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)
     (* Either RANDOM or a tile name (e.g., N12 or S34) *)

done;;
