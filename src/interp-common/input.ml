
exception ExtractionException

module Kind = struct
  type _ t =
    | KBool : bool t
    | KInt : int t

  let zero (type a) (kind : a t) : a =
    match kind with KInt -> 0 | KBool -> false

  let rand (type a) (kind : a t) : a =
    match kind with
    | KInt -> Rand.int_incl (-10) 10
    | KBool -> Rand.bool ()
end

type t =
  | IBool of bool
  | IInt of int

let extract_exn (type a) (kind : a Kind.t) (input : t) : a =
  match kind, input with
  | KBool, IBool b -> b
  | KInt, IInt i -> i
  | _ -> raise ExtractionException

let to_string = function
  | IBool b -> Bool.to_string b
  | IInt i -> Int.to_string i

let input_conv : t Cmdliner.Arg.Conv.t =
  let parser = function
    | "true" -> Ok (IBool true)
    | "false" -> Ok (IBool false)
    | s ->
      match int_of_string_opt s with
      | Some i -> Ok (IInt i)
      | None -> Error (Format.sprintf "Failed to parse an input from '%s'." s)
  in
  let pp formatter i =
    Format.fprintf formatter "%s" (to_string i)
  in
  Cmdliner.Arg.Conv.make ~parser ~pp () ~docv:"INPUT"

let parse_list =
  let open Cmdliner.Arg in
  value & opt (list ~sep:' ' input_conv) [] & info ["inputs"] ~doc:"Input list for interpreter"
