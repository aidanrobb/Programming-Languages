let welcome = "Welcome to the Oakland, California Municipal Library (OCaML)"

(* These types are defined for you. You should not modify them *)
type catalog_item = Book of string * string | Movie of string * int | CD of string * string | Computer
type checkout_entry = Item of catalog_item | New of checkout_entry | Pair of checkout_entry * checkout_entry
type checkout = Checkout of checkout_entry * int option

(* Examples *)
(* These are some examples of checkout_item. You should test locally with these before submitting *)
let i0 = Book ("Types and Programming Languages", "Benjamin Pierce")
let i1 = Movie ("The Imitation Game", 2014)
let i2 = Computer

(* These are some examples of checkout_entry. You should test locally with these before submitting *)
let e0 = Item i0
let e1 = Item i1
let e2 = Item i2

let e3 = Item (CD ("Songs to Test By", "Aperture Science Psychoacoustic Laboratories"))
let e4 = New (Item (Book ("UNIX: A History and a Memoir", "Brian W. Kernighan")))

let e5 = Pair (
    Item (Movie ("WarGames", 1983)),
    Item (Movie ("Sneakers", 1992))
)

let e6 = Pair (
    Pair (
        Item (Book ("The Unix Programming Environment", "Brian W. Kernighan and Rob Pike")),
        Item (Book ("The C Programming Language", "Brian Kernighan and Dennis Ritchie"))
    ),
    Item (Book ("The AWK Programming Language", "Alfred V. Aho, Brian W. Kernighan, and Peter J. Weinberger"))
)

(* This is an exmaple of a checkout list. You should test locally with it before submitting *)
let checked_out = [Checkout (e1, Some 2); Checkout (e2, None); Checkout (e4, Some 1); Checkout (e5, Some 2)]

(* The following functions you must implement *)

(* Display item as string *)
let string_of_item (i : catalog_item) : string =
  match i with
  Computer -> "Public Computer"
  |Book (bName, author) -> bName ^ " by " ^ author
  |Movie (mName, year) -> mName ^ " (" ^ string_of_int year ^ ")"
  |CD (album, artist) -> album ^ " by " ^ artist


(* Display entry as string *)
let rec string_of_entry (e : checkout_entry) : string =
  match e with
  Item item -> string_of_item item
  |New newItem -> "(NEW) " ^ string_of_entry newItem
  |Pair (item1, item2) -> string_of_entry item1 ^ " and " ^ string_of_entry item2

(* Return the daily fine for an overdue item *)
let rec daily_fine (entry: checkout_entry) : float =
  match entry with
  | New doubleFine -> 2.0 *. daily_fine doubleFine
  | Pair (fine1, fine2) -> daily_fine fine1 +. daily_fine fine2
  |Item normalFine ->
  match normalFine with
  | Book (e1,e2) -> float_of_string ".25"
  | Movie (e1,e2) -> float_of_string ".5"
  | CD (e1,e2) -> float_of_string ".5"
  |Computer -> float_of_string "0.0"

(* Given a list of items and days overdue, compute the total fine *)
let rec total_fine (l : checkout list) : float =
  match l with
  |[] -> 0.0
  | Checkout (itemCost, daysOverdue) :: tail ->
  match daysOverdue with
  |None -> total_fine tail
  | Some days -> daily_fine itemCost *. float_of_int days +. total_fine tail
