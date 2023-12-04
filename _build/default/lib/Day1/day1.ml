open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let rec filter chars acc = 
    let digits = "0123456789" in 
    match chars with 
    | [] -> List.rev acc
    | hd :: tl ->
            if String.contains digits hd then filter tl (hd :: acc)
            else filter tl acc
;;


let rec last_element_of_list str = 
    let list = explode str in 
    match list with 
    | [] -> "" 
    | [x] -> String.of_char x
    | _ :: tl -> 
            let new_tail = String.of_char_list tl in 
            last_element_of_list new_tail 
;;

let first_element_of_list str =
    let exploded = explode str in 
    match exploded with 
    | [] -> "" 
    | hd :: _ -> String.of_char hd
;;

let filter_only_ints str =
    let list = explode str in 
    let filtered_list = filter list [] in
    String.of_char_list filtered_list
;;    

let concatenate_numbers str = 
    let first_element = first_element_of_list str in 
    let last_element = last_element_of_list str in 
    String.concat ~sep:"" [first_element; last_element]
;;

let sum_all list = 
    let numbered_list = List.map ~f: int_of_string list in
    List.fold ~init:0 ~f:(+) numbered_list 

let _ = 
let input = read_lines "./lib/Day1/input.txt" in 
    let parsed_inputs = List.map ~f:filter_only_ints input in 
    let concatenated_numbers = List.map ~f:concatenate_numbers parsed_inputs in
    let all_summed = sum_all concatenated_numbers in
    Printf.printf "Result = %i" all_summed 
;;
