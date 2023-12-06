open Core;;


let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let get_game_info game = 
    assert false

let () = 
    let red_amount = 12 in 
    let green_amount = 13 in 
    let blue_amount = 14 in 

    let file = read_lines "./lib/Day2/input.txt" in
    let _ = List.iter file ~f
