let pangram (inFile : string) (outFile : string) : unit =

  (* Here we open an input channel for first argument, inFile,
     and bind it to a variable ic so that we can refer it
     later in loop_read function. *)
  let ic = open_in inFile in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out outFile in

  (* Helper function: file input function. It reads file line by line
     and return the result as a list of string.  *)
  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the
           head of old list. *)
      | End_of_file -> List.rev acc in

  (* Helper function: file output function. It takes a bool value and
     write it to the output file. *)
  let file_write bool_val = Printf.fprintf oc "%b\n" bool_val in

  (* This variable contains the result of input file from helper
     function, loop_read. Please remember this is a list of string. *)
  let ls_str = loop_read [] in

  (* ***** Code From Here, Replace () above and write your code ***** *)
  let charlist =  ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in

  let rec stringCheck word charlist =
    match charlist with
     [] -> file_write true
    | firstChar :: otherChars -> if String.contains word firstChar then stringCheck word otherChars else file_write false in

    let rec parsStringList stringList charList =
      match stringList with
        [] -> ()
        | topString:: nextStrings -> stringCheck topString charList ; parsStringList nextStrings charList in

    if List.length ls_str != 0 then
        match ls_str with
        [] -> ()
        | firstString :: otherStings -> stringCheck firstString charlist; parsStringList otherStings charlist ;

(* Do not include this in your submission but use this line to test your code *)
