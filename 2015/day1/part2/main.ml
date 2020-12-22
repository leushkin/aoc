open Core

exception UnexpecedChar of char
exception SomethingWentWrong of string

let char_list_of_string s = List.init (String.length s) ~f:(String.get s)

let read_file (file_path : string) : string =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let helper = function
  | Some x -> (
      match x with
      | '(' -> 1
      | ')' -> -1
      | _ -> UnexpecedChar x |> raise
    )
  | None -> SomethingWentWrong "funciton got None" |> raise

let solution (str: char list) : int =
  let i = ref 0 in
  let acc = ref 0 in
  while !acc != -1 do
    acc := !acc + (!i |> List.nth str |> helper);
    i := !i + 1
  done;
  !i

let () =
  let args = () |> Sys.get_argv in
  if (=) (args |> Array.length) 2
  then
    let input = args.(1) |> read_file |> char_list_of_string in
    solution input |> string_of_int |> print_string
  else print_string "Usage: ./solution <file_name>"


