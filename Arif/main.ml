open Format
open Lexing
open Error

let usage = "usage: interpret [options] file.ml"

let interpret = ref true

let spec = [ "-p", Arg.Clear interpret, "  parse only" ]

let file = 
  let file = ref None in
  let set_file s =  
    if not (Filename.check_suffix s ".ml") then 
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.prog Lexer.token lb in
    close_in c;
    if !interpret
    then Interpreter.interpret_prog p
    else eprintf "Syntaxe OK\n@.";
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
