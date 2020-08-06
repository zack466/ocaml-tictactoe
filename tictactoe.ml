open List
open Char
open Bool
open Scanf
open Printf

let board = [['.'; '.'; '.']; ['.'; '.'; '.']; ['.'; '.'; '.']]

let get_rc board r c = nth (nth board r) c

let print_rc r c = print_endline (escaped (get_rc board r c))

(* let () = print_rc 0 0 *)

let rec replace lst ele idx = match lst with
    | [_] -> [ele]
    | hd :: tl -> if idx = 0 then (ele :: tl) else (hd :: replace tl ele (idx-1))
    | [] -> assert false

let make_move board ch r c =
    let row = nth board r in
    let row = replace row ch c in
    replace board row r

let rec print_board board = match board with
    | [] -> ()
    | hd :: tl -> iter (fprintf Stdlib.stdout "%c ") hd; print_endline ""; print_board tl

(* let () = print_board board *)

let char_equal c1 c2 = 
    code c1 = code c2

let bool_or b1 b2 = 
    b1 || b2

let bool_and b1 b2 =
    b1 && b2

let check_board board ch =
    let rec check_rows board ch = match board with
        | [] -> false
        | hd :: tl -> let bools = map (char_equal ch) hd in
                        let result = fold_left bool_and true bools in
                        result || (check_rows tl ch)
    in
    let check_cols board ch =
        let rec helper board idx ch =
            if idx = 3 then false else
                let col = map (fun f -> f idx) (map nth board) in
                let bools = map (char_equal ch) col in
                let result = fold_left bool_and true bools in
                result || (helper board (idx+1) ch)
        in
        helper board 0 ch
    in
    let check_diags board ch = 
        let charat r c = get_rc board r c in
        let diag1 = 
            char_equal (charat 0 0) (ch)
            &&
            char_equal (charat 0 0) (charat 1 1)
            &&
            char_equal (charat 1 1) (charat 2 2)
        in
        let diag2 = 
            char_equal (charat 2 0) (ch)
            &&
            char_equal (charat 2 0) (charat 1 1)
            &&
            char_equal (charat 1 1) (charat 0 2)
        in
        diag1 || diag2
    in
    (*
    let () = if check_rows board ch then let () = print_endline "row" in () else () in
    let () = if check_cols board ch then let () = print_endline "cols" in () else () in
    let () = if check_diags board ch then let () = print_endline "diags" in () else () in
    *)    
    check_rows board ch || check_cols board ch || check_diags board ch

let rec get_input board =
    try
        let r,c = scanf "%d %d\n" (fun a b -> (a-1,b-1)) in
        if (r >= 0 && r <= 2) && (c >= 0 && c <= 2) && (char_equal (get_rc board r c) '.' ) then r,c
        else let () = print_endline "Invalid move" in get_input board
    with e ->
        let () = scanf "%s\n" (fun a -> ()); print_endline "Invalid input"; in
        get_input board

let play () =
    print_endline "To make a move, enter the row and column numbers. Ex: '2 2' for middle square";
    let rec loop board ch turn =
        print_board board;
        let r,c = get_input board in
        print_endline "";
        let board = make_move board ch r c in
        let victory = check_board board ch in
        if victory then let () = print_board board; in (escaped ch) ^ " has won!"
        else if turn = 9 then let () = print_board board; in "Its a tie!"
        else if (char_equal ch 'X') then loop board 'O' (turn+1)
        else loop board 'X' (turn+1)
    in
    let results = loop board 'X' 1 in
    print_endline results

let () = play ()
