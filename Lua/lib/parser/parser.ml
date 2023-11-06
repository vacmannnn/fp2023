open! Base

let parse str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Stat.parse_block str

let parse_exn str = Result.ok_or_failwith (parse str)
