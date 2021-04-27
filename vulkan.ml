type tile = N of int | S of int
let oppType = function
    | N(i) -> S i
    | S(i) -> N i
let tile s = let i = int_of_string @@ String.sub s 1 2 in (if s.[0] = 'N' then N i else S i)
let toString = function
	| N i -> "N"^(if i<10 then "0" else "")^string_of_int(i)
    | S i -> "S"^(if i<10 then "0" else "")^string_of_int(i);;

let opp s = (if s.[0] = 'N' then "S" else "N") ^ String.sub s 1 2 in
(* Win by building an unbroken chain of volcanoes from one side of the board to the other. *)

let numberoftiles = int_of_string (input_line stdin) in (* Number of tiles on the board *)

let scoreTable = Hashtbl.create(numberoftiles) and
    indexTable = Hashtbl.create(numberoftiles) and
    nameTable = Hashtbl.create(numberoftiles) and
    neighborTable = Hashtbl.create(numberoftiles) in
let getScore i = Hashtbl.find scoreTable i and
    toIndex s = Hashtbl.find indexTable s and
    toName s = Hashtbl.find nameTable s in
for i = 0 to numberoftiles - 1 do
    (* tilename: Name of the tile at this index (e.g., N01 or S25) *)
    (* neighbor1: Index of a neighboring tile *)
    (* neighbor2: Index of a neighboring tile *)
    (* neighbor3: Index of a neighboring tile *)

    let tilename, neighbor1, neighbor2, neighbor3 = Scanf.sscanf (input_line stdin) " %s  %d  %d  %d" (fun tilename neighbor1 neighbor2 neighbor3 -> (tilename, neighbor1, neighbor2, neighbor3)) in
    Hashtbl.add scoreTable i (0,0);
    Hashtbl.add indexTable tilename i;
    Hashtbl.add nameTable i tilename;
    Hashtbl.add neighborTable i [neighbor1; neighbor2 ; neighbor3]
done;
let oppTable = let r = Hashtbl.create 80 in
    for i = 0 to numberoftiles - 1 do
        Hashtbl.add r i @@ toIndex @@ opp @@ toName i;
    done;
    r (*and
    mapHashKeys t f = let r = Hashtbl.create @@ Hashtbl.length t in
        Hashtbl.iter (fun k e -> Hashtbl.add r (f k) e) t;
        r*) in
let oppIndex i = Hashtbl.find oppTable i in



(* game loop *)
let getDoubleAndSimple g =
    let rec aux accDouble double simple = function
        | [] -> double, simple
        | x :: xs -> let oppX = oppIndex x in (if List.mem x accDouble then aux accDouble double simple else
            if List.mem oppX xs then
                aux (oppX :: accDouble) ((x, oppX) :: double) simple
            else aux accDouble double @@ x:: simple) xs
    in aux [] [] [] g and
    getMyPositions pos =
        let rec aux acc i = function
            | [] -> acc
            | x :: xs -> (if x > 0 then aux (i :: acc) (i+1) else aux acc (i+1)) xs
        in aux pos 0 [] in


let startScore = 100. in
let queue = Queue.create() and
    minQueue = ref startScore in

let flipflop = ref true in
while true do
    let t = Sys.time() in
    let position = List.map int_of_string @@ String.split_on_char ' ' @@ input_line stdin and (* Space-separated list of the volcano levels for every tile on the board (in index order); value will be positive for your volcanoes, negative for your opponent's volcanoes, or 0 if empty *)
        moves = String.split_on_char ' ' @@ input_line stdin in (* Space-separate list of all valid moves in this position *)
    let intMoves = List.map toIndex moves and
        doubleGoals, simpleGoals = getDoubleAndSimple @@ getMyPositions position in




    while Sys.time() - t > 0.09 do
        let getOld, getNew = if (!flipflop then fst,snd else snd, fst) and
            set (a, b) s = if (!flipflop then (a,s) else (s,b)) in
        let addGains i amount =
            if let score = getScore i in getNew score < amount && getOld score then (

            )
        List.iter addGains simpleGoals;
    done;

    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)
    print_endline "random"; (* Either RANDOM or a tile name (e.g., N12 or S34) *)
    ();
done;;
