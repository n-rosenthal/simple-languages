(**
  functions for drawing boxes for terminal-based applications
*)

type boxComponent = 
  | SupLeftCorner
  | SupRightCorner
  | InfLeftCorner
  | InfRightCorner
  | HorizontalLine
  | VerticalLine
  | RightCross
  | LeftCross
  | TopCross
  | BottomCross
  | CenterCross
;;

let string_of_boxComponent (c: boxComponent) : string = (match c with
  | SupLeftCorner -> "╔"
  | SupRightCorner -> "╗"
  | InfLeftCorner -> "╚"
  | InfRightCorner -> "╝"
  | HorizontalLine -> "═"
  | VerticalLine -> "║"
  | RightCross -> "╠"
  | LeftCross -> "╣"
  | TopCross -> "╦"
  | BottomCross -> "╩"
  | CenterCross -> "╬"
);;

let length (s: string) : int =
  String.length s
;;

let split_at (n: int) (s: string) : (string * string) =
  (String.sub s 0 n, String.sub s n (length s - n))
;;

let chars_of_string (s: string) : char list =
  List.map (fun c -> c) (String.to_seq s |> List.of_seq)
;;

let string_of_chars (cs: char list) : string =
  String.concat "" (List.map (fun c -> String.make 1 c) cs)
;;

let break_string (s: string) (size: int) : string list =
  let rec aux (s: string) (acc: string list) : string list =
    if (length s) <= size then
      s :: acc
    else
      let (s1, s2) = split_at size s in
      aux s2 (s1 :: acc)
  in
  aux s []
;;

(* repeat a string n times *)
let rec repeat (s: string) (n: int) : string =
  if n <= 0 then
    ""
  else
    s ^ repeat s (n-1)
;;

let get_box_top (len: int) : string =
  (string_of_boxComponent SupLeftCorner) ^ repeat (string_of_boxComponent HorizontalLine) len ^ (string_of_boxComponent SupRightCorner)
;;

let get_box_bottom (len: int) : string =
  (string_of_boxComponent InfLeftCorner) ^ repeat (string_of_boxComponent HorizontalLine) len ^ (string_of_boxComponent InfRightCorner)
;;

let wrap (s: string) (border: int) (maxsize: int) : string list =
  let top = get_box_top maxsize in
  let bottom = get_box_bottom maxsize in
  let lines = break_string s (maxsize - 2*border) in
  let middle = List.map (fun s -> (string_of_boxComponent VerticalLine) ^ (repeat " " border) ^ s ^ (repeat " " border) ^  (string_of_boxComponent VerticalLine)) lines in
  [top] @ middle @ [bottom]
;;
let box = wrap "são com pequenos gestos que a vida se torna mais doce, mais amával, mais fácil de se levar, é, no dia-a-dia as coisas são difíceis, se não há amor, carinho, compreensão, respeito né, principalmente." 2 36 ;;

