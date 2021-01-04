type 'a str = Cons of 'a * ('a stream) | Nil
and  'a stream = unit -> 'a str

exception Subscript
exception Empty

let head (s :'a stream) : 'a =
  match s () with
    Cons (hd,tl) -> hd
  | Nil -> raise Empty

let tail (s :'a stream) : 'a stream =
  match s () with
    Cons (hd,tl) -> tl
  | Nil -> raise Empty

let null (s : 'a stream) =
  match s () with
    Nil -> true
  | _ -> false

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
    n when n > 0 -> head s :: take (n - 1) (tail s)
  | 0 -> []
  | _ -> raise Subscript

let rec nth (n: int) (s: 'a stream) : 'a =
  match n with
    n when n > 0 -> nth (n - 1) (tail s)
  | 0 -> head s
  | _ -> raise Subscript

let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))

let rec filter (s: 'a stream) (f: 'a -> bool) : 'a stream =
  if f (head s)
  then fun () -> Cons (head s, filter (tail s) f)
  else filter (tail s) f

let rec sieve (s: int stream) : int stream =
  fun () -> Cons(head s, sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))

let rec fromn (n: int) = fun () -> Cons (n, fromn (n + 1))
let rec fib n m = fun () -> Cons (n, fib m (n+m))
let rec square n = fun () -> Cons (n*n, square (n+1))
let rec simple n = fun () -> Cons (n, simple(n+1))


(* implement the streams and functions below *)
let even : int -> bool = fun x -> if x mod 2 == 0 then true else false
let odd : int -> bool = fun x -> if x mod 2 == 1 then true else false

let rec evenfib (n:int) (m:int) = if n mod 2 == 0 then fun () -> Cons(n,evenfib m (n+m)) else evenfib m (n+m)
let rec oddfib (n:int) (m:int) = if n mod 2 == 1 then fun () -> Cons(n,oddfib m (n+m)) else oddfib m (n+m)


let squares : int stream = fun () -> Cons (1, fun () -> Cons(4,square 3))
let fibs : int stream = fun () ->  Cons (0, fun () -> Cons (1, fib 1 2) )
let evenFibs : int stream = fun () -> Cons (0, evenfib 1 1)
let oddFibs : int stream = fun () -> Cons (1, oddfib 1 2)
let primes : int stream = sieve (fun () -> Cons(2, simple 3))


let rec simpleZip a b f n = fun () -> Cons (((nth n b),(nth n a),(f(nth n b, nth n a))), simpleZip a b f (n+1) )

let rev_zip_diff : 'a stream -> 'b stream -> ('b * 'a -> 'c) -> ('b * 'a * 'c) stream =
  fun a b f -> fun () -> Cons (((nth 0 b),(nth 0 a ),f(nth 0 b, nth 0 a)), simpleZip a b f 1)

let rec printGenList : 'a list -> ('a -> unit) -> unit =
  fun l f -> match l with
        | [] -> ()
        |elem::restofList -> f elem ; printGenList restofList f

let rec printNum numlist =
        match numlist with
          [] -> ""
          |elem::restofList -> string_of_int elem ^ " " ^ printNum restofList

let rec printList : int list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    printGenList l (fun num -> Printf.fprintf oc "%s" (string_of_int num ^ " "));
    close_out oc

let rec printPairList : (int * int) list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    printGenList l (fun (num1,num2) -> Printf.fprintf oc "%s" ( "("^ string_of_int num1 ^ ", "^ string_of_int num2 ^ ") "));
    close_out oc
;;
printPairList [(2, 1); (3, 2); (4, 3)] "printPairList.txt"
