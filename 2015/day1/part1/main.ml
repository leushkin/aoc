open Core
open Sys

exception Unexpected of string

let char_list_of_string s = List.init (String.length s) (String.get s)

let rec solution (str : char list) (acc : int) : int =
  let helper x xs = match x with
    | '(' -> solution xs (acc + 1)
    | ')' -> solution xs (acc - 1)
    | _ -> Unexpected (Char.to_string x) |> raise
  in
  match str with
  | x :: xs -> helper x xs
  | [] -> acc

let read_file (file_path: string): string =
  let ch = open_in file_path in
  let n = in_channel_length ch in
  let s = really_input_string ch n in
  close_in ch;
  s

let () =
  let args = () |> Sys.get_argv in
  if (=) (args |> Array.length) 2
  then
    let input = args.(1) |> read_file |> char_list_of_string in
    solution input 0 |> string_of_int |> print_string
  else print_string "Usage: ./solution <file_name>"