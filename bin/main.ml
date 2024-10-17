let rec last l = 
    match l with
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;

match last ["a"; "b"; "c"] with
| None -> print_endline "empty list"
| Some item -> print_endline item

let rec last_two l = 
    match l with
    | [] -> None
    | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: t -> last_two t;;

match last_two ["a"; "b"; "c"] with
| None -> print_endline "empty list"
| Some (x, y) -> print_string (x ^ " " ^ y ^ "\n")

let rec nth l n =
    match l, n with 
    | h :: _, 0 -> Some h
    | _ :: t, n when n > 0 -> nth t (n - 1)
    | _ -> None;;

match nth ["a"; "b"; "c"] 2 with
| None -> print_endline "can't find nth element"
| Some item -> print_endline item

let length l = 
    let rec len l acc =
        match l with 
        | [] -> acc
        | _ :: t -> len t (acc + 1)
    in
    len l 0;;

Printf.printf "%d\n" (length ["a"; "b"; "c"; "d"])

let rev l =
    let rec aux l acc =
        match l with 
        | [] -> acc
        | h :: t -> aux t (h :: acc)
    in
    aux l [];;

let () = List.iter (Printf.printf "%s ") (rev ["a"; "b"; "c"; "d"]);;
Printf.printf "\n";;

let is_palindrome l =
    let rec aux a b =
        match a, b with
        | [], [] -> true
        | ah :: at, bh :: bt when ah == bh -> aux at bt
        | _ -> false
    in
    aux l (rev l);;

Printf.printf "%b\n" (is_palindrome ["a"; "a"])
