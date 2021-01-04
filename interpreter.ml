(* Defined Data Types*)
type value = Integer of int | String of string | Boolean of string | Error of string | Unit of string | Name of string | Nothing| FuncBound | Closure of (string * (value * value) list * string list * bool)

(*interpreter*)
let interpreter ((input:string), (output:string)): unit =

(*Deals with file parsing and input*)
let inputString = open_in input in
let outputString = open_out output in
let rec loop_read acc =
    try
      let l = String.trim(input_line inputString) in
      loop_read (l::acc)
    with
    | End_of_file -> List.rev acc in
let file_write stringElem = Printf.fprintf outputString "%s\n" stringElem
in
let ls_str = loop_read []
in
let stackList = []
in
let env = []
in
(*Helper function to parse a string properly*)
let trimString stringToTrim =
  let trimedString = String.split_on_char '"' stringToTrim  in
  match trimedString with
  push::goodString::nothing -> goodString
  |_ -> ""
in
(*Helper function to check to see if a name starts with a letter*)
let is_alpha alpha =
    match alpha with
      'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
in
(*Helper function to check to see if a string is an int*)
 let check_str s =
    try int_of_string s |> ignore; true
    with Failure _ -> false
in
(*Helper function to see if a  string contains a substring*)
let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true
in
(*Helper function to see if there exists a key in the env*)
let rec exists elem env =
    match env with
    [] -> false
    |(Name(name1),value)::restOfList -> if contains name1 elem then true else exists elem restOfList
    |(value1,value2)::restOfList -> exists elem restOfList
in
(*Helper function to see if value is an int in env*)
let rec isInt elem env =
  match env with
  [] -> false
  |(Name(name1),value)::restOfList -> if contains name1 elem then
  (match value with
    Integer(intVal) -> true
    |_ -> false
  )
  else isInt elem restOfList
  |(value1,value2)::restOfList -> isInt elem restOfList
in
(*Helper function to see if value is an string in env*)
let rec isString elem env =
  match env with
  [] -> false
  |(Name(name1),value)::restOfList -> if contains name1 elem then
  (match value with
    String(stringVal) -> true
    |_ -> false
  )
  else isString elem restOfList
  |(value1,value2)::restOfList -> isString elem restOfList
in
(*Helper function to see if value is an bool in env*)
let rec isBool elem env =
  match env with
  [] -> false
  |(Name(name1),value)::restOfList -> if contains name1 elem then
  (match value with
    Boolean(intVal) -> true
    |_ -> false
  )
  else isBool elem restOfList
  |(value1,value2)::restOfList -> isBool elem restOfList
in
(*Helper function to get value from env*)
let rec getInt elem env =
  match env with
  []->0
  |(Name(name1),Integer(int1))::restOfList -> if contains name1 elem then int1 else getInt elem restOfList
  |(value1,value2)::restOfList -> getInt elem restOfList
in
(*Helper function to get value from env*)
let rec getString elem env =
  match env with
  []-> ""
  |(Name(name1),String(string1))::restOfList -> if contains name1 elem then string1 else getString elem restOfList
  |(value1,value2)::restOfList -> getString elem restOfList
in
(*Helper function to get value from env*)
let rec getBool elem env =
  match env with
  []-> ""
  |(Name(name1),Boolean(bool1))::restOfList -> if contains name1 elem then bool1 else getBool elem restOfList
  |(value1,value2)::restOfList -> getBool elem restOfList
in
(*Helper function to get any value from env*)
let rec getValue name env =
  match env with
  []-> Nothing
  |(Name(name1), value)::restOfList -> if contains name1 name then value else getValue name restOfList
  |(value1,value2)::restOfList -> getValue name restOfList
in
(*Trims the list to get rid of all unscoped elems*)
let rec trimStack stack =
match stack with
[]-> []
| Nothing::restOfStack -> restOfStack
| badvalue::restOfStack-> trimStack (restOfStack)
in
(*Trims the env to get rid of all unscoped elems*)
let rec trimEnv env =
match env with
[]-> []
| (Nothing, Nothing)::restOfEnv -> restOfEnv
| (value1,value2)::restOfEnv-> trimEnv restOfEnv
in
(* Trims command list for functions*)
let rec trimRemCom cmdList =
match cmdList with
| [] -> []
| currCom::remCom -> if contains currCom "funEnd" then remCom else trimRemCom remCom
in
(*Gets arg for func *)
let getArg funcString =
let funArg = String.split_on_char ' ' funcString in
match funArg with
| funC::name::arg::[] -> arg
| _ -> "[]"
in
(*Gets name for func *)
let getName funcString =
let funName11 = String.split_on_char ' ' funcString in
match funName11 with
| funC::name::arg::[] -> name
| _ -> "[]"
in
(*Gets body of a func*)
let rec getBody remCom bodyList=
match remCom with
| [] -> []
| currCom::remCom -> if contains currCom "funEnd" then bodyList else currCom::(getBody remCom bodyList)
in
(*binds arg*)
let rec getArgName funcCall env  =
 match env with
 | (Name(funName),Closure(arg,env1,cmdList,inOut))::restOfEnv -> if contains funName funcCall then Name(arg) else  getArgName funcCall restOfEnv
 | []-> Nothing
 | value::restOfEnv-> getArgName funcCall restOfEnv
in
(*Get current func's closure*)
let rec getClosureEnv funcCall env =
match env with
| (Name(name1),Closure(arg,env1,cmdList,inOut)):: restOfEnv -> if contains name1 funcCall then env1 else getClosureEnv funcCall restOfEnv
|[] -> []
| value::restOfEnv -> getClosureEnv funcCall restOfEnv
in
(*Get current func's cmdList*)
let rec getCmdList funcCall env =
match env with
| (Name(name1),Closure(arg,env1,cmdList,inOut)):: restOfEnv -> if contains name1 funcCall then cmdList else getCmdList funcCall restOfEnv
|[] -> []
| value::restOfEnv -> getCmdList funcCall restOfEnv
in
(*Get current func's bool value*)
let rec getFunBool funcCall env =
match env with
| (Name(name1),Closure(arg,env1,cmdList,inOut)):: restOfEnv -> if contains name1 funcCall then inOut else getFunBool funcCall restOfEnv
|[] -> false
| value::restOfEnv -> getFunBool funcCall restOfEnv
in
(*trims stack from function*)
let rec funTrimStack stackList =
match stackList with
| FuncBound::value2 -> value2
|value::value2 -> funTrimStack value2
|[]->[]
in
(* Implements PUSH funcationality to the interpreter*)
let push pushElement (stackList,env) =
  if String.get pushElement 5 == '"' then
  (String(trimString pushElement)::stackList,env) else
  let parsList = String.split_on_char ' ' pushElement in
  match parsList with
  pushCom::pushElem::[] -> if check_str pushElem then (Integer(int_of_string pushElem)::stackList,env) else
                           if contains pushElem ":true:" || contains pushElem ":false:" then (Boolean(pushElem)::stackList,env) else
                           if contains pushElem ":error:" then (Error(pushElem)::stackList,env) else
                           if contains pushElem ":unit:" then (Unit(pushElem)::stackList,env) else
                           if String.get pushElem 0 == '_' || is_alpha (String.get pushElem 0) then (Name(pushElem)::stackList,env) else
                           (Error(":error:")::stackList,env)
  | _ -> (Error(":error:")::stackList,env)
  in
(*Implements POP funcationality to the interpreter*)
let pop (stackList,env) =
 match stackList with
 []-> (Error(":error:")::stackList,env)
 |headList::remList -> (remList,env)
in
(*Implements ADD funcationality to the interpreter*)
 let add (stackList,env) =
 match stackList with
 [] -> (Error(":error:")::stackList,env)
 |oneElem::[] -> (Error(":error:")::stackList,env)
 |Integer(firstElem)::Integer(secondElem)::restOfList -> (Integer(firstElem+secondElem)::restOfList,env)
 |Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env then (Integer((getInt firstName env)+secInt)::restOfList,env) else (Error(":error:")::stackList,env)
 |Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env then (Integer((getInt secName env)+firstInt)::restOfList,env) else (Error(":error:")::stackList,env)
 |Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env then (Integer((getInt firstName env)+(getInt secName env))::restOfList,env) else (Error(":error:")::stackList,env)
 |_-> (Error(":error:")::stackList,env)
in
(*Implements SUB funcationality to the interpreter*)
let sub (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|oneElem::[] -> (Error(":error:")::stackList,env)
|Integer(firstElem)::Integer(secondElem)::restOfList -> (Integer(secondElem - firstElem)::restOfList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env then (Integer(secInt - (getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env then (Integer((getInt secName env) - firstInt)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env then (Integer((getInt secName env)-(getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|_-> (Error(":error:")::stackList,env)
in
(*Implements MUL funcationality to the interpreter*)
let mul (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|oneElem::[] -> (Error(":error:")::stackList,env)
|Integer(firstElem)::Integer(secondElem)::restOfList -> (Integer(firstElem * secondElem)::restOfList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env then (Integer(secInt * (getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env then (Integer((getInt secName env) * firstInt)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env then (Integer((getInt secName env)*(getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|_-> (Error(":error:")::stackList,env)
in
(*Implements DIV funcationality to the interpreter*)
let div (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|oneElem::[] -> (Error(":error:")::stackList,env)
|Integer(firstElem)::Integer(secondElem)::restOfList -> if firstElem != 0  then (Integer(secondElem / firstElem)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env && getInt firstName env != 0 then (Integer(secInt / (getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env && firstInt != 0 then (Integer((getInt secName env) / firstInt)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env && getInt firstName env != 0 then (Integer((getInt secName env)/(getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|_-> (Error(":error:")::stackList,env)
in
(*Implements REM funcationality to the interpreter*)
let remain (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|oneElem::[] -> (Error(":error:")::stackList,env)
|Integer(firstElem)::Integer(secondElem)::restOfList -> if firstElem != 0  then (Integer(secondElem mod firstElem)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env && getInt firstName env != 0 then (Integer(secInt mod (getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env && firstInt != 0 then (Integer((getInt secName env) mod firstInt)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env && getInt firstName env != 0 then (Integer((getInt secName env) mod (getInt firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|_-> (Error(":error:")::stackList,env)
in
(*Implements NEG funcationality to the interpreter*)
let negate (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|Integer(oneElem)::restOfList -> if oneElem == 0 then (stackList,env) else
  if oneElem > 0 then (Integer(oneElem - oneElem - oneElem)::restOfList,env) else
  (Integer(abs oneElem)::restOfList,env)
|Name(name1)::restOfList -> if exists name1 env && isInt name1 env then
  (if getInt name1 env == 0 then (Integer(0)::restOfList,env) else
  if getInt name1 env > 0 then (Integer((getInt name1 env)- (getInt name1 env) - (getInt name1 env))::restOfList,env) else
  (Integer(abs (getInt name1 env))::restOfList,env)
  )
  else (Error(":error:")::stackList,env)
|_-> (Error(":error:")::stackList,env)
in
(*Implements SWAP funcationality to the interpreter*)
let swap (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|oneElem::[] -> (Error(":error:")::stackList,env)
|firstElem::secondElem::restOfList -> (secondElem::firstElem::restOfList,env)
in
(*Implements TOSTRING funcationality to the interpreter*)
let toString (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|Integer(elem1)::remList-> if elem1 >= 0 then (String(string_of_int elem1)::remList,env) else  (String("-" ^ string_of_int (abs elem1))::remList,env)
|Boolean(elem2)::remList-> (String(elem2)::remList,env)
|Error(elem3)::remList-> (String(elem3)::remList,env)
|Unit(elem4)::remList-> (String(elem4)::remList,env)
|String(elem5)::remList-> (stackList,env)
|Name(elem6)::remList-> (String(elem6)::remList,env)
|_ ->(stackList,env)
in
(*Implements PRINTLN funcationality to the interpreter*)
let println (stackList,env) =
match stackList with
[] -> (Error(":error:")::stackList,env)
|String(stringElem)::remList -> file_write stringElem; (remList,env)
|_ -> (Error(":error:")::stackList,env)
in
(*Implements CAT funcationality*)
let cat (stackList,env) =
match stackList with
| String(elem1)::String(elem2)::restOfList -> (String(elem2^elem1)::restOfList,env)
|Name(firstName)::String(secString)::restOfList -> if exists firstName env && isString firstName env then (String(secString^(getString firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
|String(firstString)::Name(secName)::restOfList-> if exists secName env && isString secName env then (String((getString secName env)^firstString)::restOfList,env) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isString firstName env && exists secName env && isString secName env then (String((getString secName env)^(getString firstName env))::restOfList,env) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements AND funcationality*)
let logand (stackList,env) =
match stackList with
|Boolean(bool1)::Boolean(bool2)::restOfList -> if (contains bool1 "true" && contains bool2 "true") then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)
|Name(firstName)::Boolean(secBool)::restOfList -> if exists firstName env && isBool firstName env then (if contains (getBool firstName env) "true" && contains secBool "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
|Boolean(firstBool)::Name(secName)::restOfList -> if exists secName env && isBool secName env then (if contains (getBool secName env) "true" && contains firstBool "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isBool firstName env && exists secName env && isBool secName env then (if contains (getBool secName env ) "true" && contains (getBool firstName env) "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements OR funcationality*)
let logor (stackList,env) =
match stackList with
| Boolean(bool1)::Boolean(bool2)::restOfList -> if (contains bool1 "true" || contains bool2 "true") then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)
|Name(firstName)::Boolean(secBool)::restOfList -> if exists firstName env && isBool firstName env then (if contains (getBool firstName env) "true" || contains secBool "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)) else (Error(":error:")::stackList,env)
|Boolean(firstBool)::Name(secName)::restOfList -> if exists secName env && isBool secName env then (if contains (getBool secName env) "true" || contains firstBool "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isBool firstName env && exists secName env && isBool secName env then (if contains (getBool secName env ) "true" || contains (getBool firstName env) "true" then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements NOT funcationality*)
let lognot (stackList,env) =
match stackList with
| Boolean(bool1)::restOfList -> if (contains bool1 "true") then (Boolean(":false:")::restOfList,env) else (Boolean(":true:")::restOfList,env)
|Name(firstName)::restOfList -> if exists firstName env && isBool firstName env then (if contains (getBool firstName env) "true" then (Boolean(":false:")::restOfList,env) else (Boolean(":true:")::restOfList,env)) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements EQUAL funcationality*)
let equal (stackList,env) =
match stackList with
| Integer(int1)::Integer(int2)::restOfList -> if (int1 == int2) then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env then (if (getInt firstName env) == secInt then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env then (if (getInt secName env) == firstInt then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env then (if (getInt firstName env) == (getInt secName env) then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements LESSTHAN funcationality*)
let lessThan (stackList,env) =
match stackList with
| Integer(int1)::Integer(int2)::restOfList -> if (int2 < int1) then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env)
|Name(firstName)::Integer(secInt)::restOfList -> if exists firstName env && isInt firstName env then (if secInt < (getInt firstName env)  then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
|Integer(firstInt)::Name(secName)::restOfList-> if exists secName env && isInt secName env then (if (getInt secName env) < firstInt then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
|Name(firstName)::Name(secName)::restOfList -> if exists firstName env && isInt firstName env && exists secName env && isInt secName env then (if (getInt secName env) < (getInt firstName env) then (Boolean(":true:")::restOfList,env) else (Boolean(":false:")::restOfList,env) ) else (Error(":error:")::stackList,env)
| _ -> (Error(":error:")::stackList,env)
in
(*Implements BIND funcationality*)
let bind (stackList,env) =
match stackList with
topElem::Name(name1)::restOfList ->
(match topElem with
| Error(errormsg) -> (Error(":error:")::stackList,env)
| Name(newName) -> if exists newName env then (Unit(":unit:")::restOfList,((Name(name1),(getValue newName env))::env)) else (Error(":error:")::stackList,env)
| String(string1)-> (Unit(":unit:")::restOfList,((Name(name1),String(string1))::env))
| Boolean(bool1)-> (Unit(":unit:")::restOfList,((Name(name1),Boolean(bool1))::env))
| _ -> (Unit(":unit:")::restOfList,((Name(name1),topElem))::env)
)
| _ -> (Error(":error:")::stackList,env)
in
let ifStmt (stackList,env) =
match stackList with
|firstElem::secondElem::Boolean(boolValue)::restOfList -> if contains boolValue "true" then (firstElem::restOfList,env) else (secondElem::restOfList,env)
|firstElem::secondElem::Name(nameVal)::restOfList -> if exists nameVal env && contains (getBool nameVal env) "true" then (firstElem::restOfList,env) else (if exists nameVal env && contains (getBool nameVal env) "false" then (secondElem::restOfList,env) else (Error(":error:")::stackList,env))
| _ -> (Error(":error:")::stackList,env)
in
(*Implements let functionality*)
let letCmd (stackList,env) =
Nothing::stackList,(Nothing,Nothing)::env
in
(*Implements end functionality*)
let endCmd (stackList,env) =
  match stackList with
  | [] -> (stackList,env)
  |elem::restOfList -> (elem::(trimStack restOfList),trimEnv env)
in

(*Implements QUIT funcationality to the interpreter*)
let quit = ()
in
(*Trims the env*)
let trimEnvList realEnv tempEnv =
match realEnv with
|(Name(origName),Name(argName))::restOfEnv -> ((Name(origName),getValue argName tempEnv)::restOfEnv)
|_ -> (realEnv)
in
(*Trims the env*)
let trimEnvList2 realEnv  =
match realEnv with
| bindVal::restOfEnv -> (restOfEnv)
|[]-> realEnv
in
(*Computes function*)
let rec computeFunc (cmdList,(stackList,tempEnv), realEnv, funBoool) =
 match cmdList with
 | [] -> if funBoool then ((funTrimStack stackList),(trimEnvList realEnv tempEnv)) else ((funTrimStack stackList),(trimEnvList2 realEnv))
 | currCom::remCom -> if contains currCom "push" then (computeFunc (remCom,(push currCom (stackList,tempEnv)),realEnv,funBoool))  else
                      if contains currCom "inOutFun" then (computeFunc ((trimRemCom remCom),((Unit(":unit:")::stackList,(Name(getName currCom),Closure(getArg currCom, env, getBody remCom [],true))::env)),realEnv,funBoool)) else
                     if contains currCom "fun " then (computeFunc ((trimRemCom remCom),((Unit(":unit:")::stackList,(Name(getName currCom),Closure(getArg currCom, env, getBody remCom [],false))::env)),realEnv,funBoool)) else
                     if contains currCom "let" then (computeFunc (remCom,(letCmd (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "end" then (computeFunc (remCom,(endCmd (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "pop"  then ( computeFunc (remCom, (pop (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "add"  then (computeFunc (remCom, (add (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "sub"  then ( computeFunc (remCom, (sub (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "mul"  then (computeFunc (remCom, (mul (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "div"  then (computeFunc (remCom, (div (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "rem"  then (computeFunc (remCom, (remain (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "neg"  then (computeFunc (remCom, (negate (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "swap"  then (computeFunc (remCom, (swap (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "toString"  then (computeFunc (remCom, (toString (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "println"  then (computeFunc (remCom, (println (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "cat"  then (computeFunc (remCom, (cat (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "and"  then (computeFunc (remCom, (logand (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "or"  then (computeFunc (remCom, (logor (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "not"  then (computeFunc (remCom, (lognot (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "equal"  then (computeFunc (remCom, (equal (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "lessThan"  then (computeFunc (remCom, (lessThan (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "bind"  then (computeFunc (remCom, (bind (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "if"  then (computeFunc (remCom, (ifStmt (stackList,tempEnv)),realEnv,funBoool)) else
                     if contains currCom "return"  then (if funBoool
                       then ( (List.hd stackList)::(funTrimStack stackList),trimEnvList realEnv tempEnv)

                     else (
                       ((List.hd stackList)::(funTrimStack stackList), trimEnvList2 realEnv)
                     ))
                    else
                     (Nothing::[],(Nothing,Nothing)::[])
in
(*Implements function calls*)
let funCall (stackList,env) =
match stackList with
| Name(argument)::Name(funcCall)::restOfStack -> if (exists funcCall env && exists argument env )then computeFunc (getCmdList funcCall env,(FuncBound::restOfStack,((getArgName funcCall env) ,(getValue argument env))::(getClosureEnv funcCall env)), ((Name(argument)) ,(getArgName funcCall env))::env, getFunBool funcCall env) else (Error(":error:")::stackList,env)
| Error(error)::funcCall::restOfStack -> (Error(":error:")::stackList,env)
| value::Name(funcCall)::restOfStack -> computeFunc (getCmdList funcCall env,(FuncBound::restOfStack,(getArgName funcCall env,(value))::(getClosureEnv funcCall env)), ((getArgName funcCall env) ,(value))::env, getFunBool funcCall env)
| _ -> (Error(":error:")::stackList,env)
in

(*Main function which executes commands*)
let rec commandRead comList (stackList,env) : unit =
match comList with
[] -> ()
| currCom::remCom -> if contains currCom "push" then (commandRead remCom (push currCom (stackList,env)))  else
                    if contains currCom "call" then (commandRead remCom (funCall (stackList,env))) else
                    if contains currCom "inOutFun" then (commandRead (trimRemCom remCom) ((Unit(":unit:")::stackList,(Name(getName currCom),Closure(getArg currCom, env, getBody remCom [],true))::env))) else
                    if contains currCom "fun" then (commandRead (trimRemCom remCom) ((Unit(":unit:")::stackList,(Name(getName currCom),Closure(getArg currCom, env, getBody remCom [],false))::env))) else
                    if contains currCom "let" then (commandRead remCom (letCmd (stackList,env))) else
                    if contains currCom "end" then (commandRead remCom (endCmd (stackList,env))) else
                    if contains currCom "pop"  then ( commandRead remCom (pop (stackList,env))) else
                    if contains currCom "add"  then (commandRead remCom (add (stackList,env))) else
                    if contains currCom "sub"  then ( commandRead remCom (sub (stackList,env))) else
                    if contains currCom "mul"  then (commandRead remCom (mul (stackList,env))) else
                    if contains currCom "div"  then (commandRead remCom (div (stackList,env))) else
                    if contains currCom "rem"  then (commandRead remCom (remain (stackList,env))) else
                    if contains currCom "neg"  then (commandRead remCom (negate (stackList,env))) else
                    if contains currCom "swap"  then (commandRead remCom (swap (stackList,env))) else
                    if contains currCom "toString"  then (commandRead remCom (toString (stackList,env))) else
                    if contains currCom "println"  then (commandRead remCom (println (stackList,env))) else
                    if contains currCom "cat"  then (commandRead remCom (cat (stackList,env))) else
                    if contains currCom "and"  then (commandRead remCom (logand (stackList,env))) else
                    if contains currCom "or"  then (commandRead remCom (logor (stackList,env))) else
                    if contains currCom "not"  then (commandRead remCom (lognot (stackList,env))) else
                    if contains currCom "equal"  then (commandRead remCom (equal (stackList,env))) else
                    if contains currCom "lessThan"  then (commandRead remCom (lessThan (stackList,env))) else
                    if contains currCom "bind"  then (commandRead remCom (bind (stackList,env))) else
                    if contains currCom "if"  then (commandRead remCom (ifStmt (stackList,env))) else
                    quit
in
commandRead ls_str (stackList,env);;
interpreter ("input23.txt","output.txt")
