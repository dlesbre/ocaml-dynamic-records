
include Signatures

module Make(O: OPERANDS)() = struct
  type t = Obj.t array ref
  type 'a unary_operand = 'a O.unary_operand
  type 'a binary_operand = 'a O.binary_operand

  type unary_wrapper = { f_unary: 'a. 'a unary_operand -> Obj.t -> 'a } [@@unboxed]
  type binary_wrapper = { f_binary: 'a. 'a binary_operand -> Obj.t -> Obj.t -> 'a } [@@unboxed]

  (** A value for extra fields *)
  let uninit_default = Obj.repr "nil"
  let uninit_unary = { f_unary=fun _ _ -> failwith "Internal error: Uninitilized unary operand"}
  let uninit_binary = { f_binary=fun _ _ _ -> failwith "Internal error: Uninitilized unary operand"}

  let initial_size = 5

  (* INVARIANT:
    - all three of these arrays always have the same size,
    - that size is greater than the number of fields [size].
    - they only have uninit_XXX values at positions greater than the number of fields [size]. *)
  let defaults = ref (Array.make initial_size uninit_default)
  let unary_operands = ref (Array.make initial_size uninit_unary)
  let binary_operands = ref (Array.make initial_size uninit_binary)
  let size = ref 0

  type update =
    | FromNil of { max:int; update: (int * Obj.t) list}
    | FromPrev of { max:int; update: (int * Obj.t) list; prev: t }

  let init = FromNil { max=0; update=[] }
  let update t = FromPrev { max = Array.length !t - 1; update = []; prev = t }

  let finish x =
    let array, update = match x with
    | FromNil {max;update} -> Array.init (max+1) (fun i -> !defaults.(i)), update
    | FromPrev {max;update;prev} -> Array.init (max+1) (fun i ->
                                      if i < Array.length !prev
                                      then !prev.(i)
                                      else !defaults.(i)), update
    in
    List.iter (fun (i,v) -> array.(i) <- v) update;
    ref array

  let copy x = ref (Array.copy !x)

  let get offset t =
    if offset < Array.length !t
    then Array.unsafe_get !t offset |> Obj.obj
    else !defaults.(offset)

  let rec unary_operand op r i acc =
    if i >= !size then acc
    else
      get i r |>
      !unary_operands.(i).f_unary op |>
      O.combine_unary op acc |>
      unary_operand op r (i+1)

  let unary_operand op r = unary_operand op r 0 (O.init_unary op)

  let rec binary_operand op l r i acc =
    if i >= !size then acc
    else
      let vl = get i l in
      let vr = get i r in
      !binary_operands.(i).f_binary op vl vr |>
      O.combine_binary op acc |>
      binary_operand op l r (i+1)
  let binary_operand op l r = binary_operand op l r 0 (O.init_binary op)

  let rec equal a b i =
    if i >= !size then true else
    get i a = get i b && equal a b (i+1)
  let equal a b = equal a b 0

  let rec compare a b i =
    if i >= !size then 0 else
    let cmp = Stdlib.compare (get i a) (get i b) in
    if cmp <> 0 then cmp else compare a b (i+1)
  let compare a b = compare a b 0

  let rec hash a i acc =
    if i >= !size then acc
    else hash a (i+1) (Hashtbl.hash (acc,get i a))
  let hash a = hash a 0 0

  module type TYPE_AND_OPERANDS = sig
    type t
    val default: t
    val unary_operand: 'a unary_operand -> t -> 'a
    val binary_operand: 'a binary_operand -> t -> t -> 'a
  end

  module MutableField(T: TYPE_AND_OPERANDS)() = struct
    type field = T.t
    type record = t
    type nonrec update = update

    let offset =
      let offset = !size in
      incr size;
      begin if Array.length !defaults >= !size
        then begin
          !defaults.(offset) <- Obj.repr T.default;
          !unary_operands.(offset) <- {f_unary=fun op r -> T.unary_operand op (Obj.obj r)};
          !binary_operands.(offset) <- {f_binary=fun op l r -> T.binary_operand op (Obj.obj l) (Obj.obj r)};
        end else begin
          let newsize = 2 * !size in
          defaults := Array.init newsize (fun i ->
            if i < offset then !defaults.(i)
            else if i = offset then Obj.repr T.default
            else uninit_default
          );
          unary_operands := Array.init newsize (fun i ->
            if i < offset then !unary_operands.(i)
            else if i = offset then {f_unary=fun op r -> T.unary_operand op (Obj.obj r)}
            else uninit_unary
          );
          binary_operands := Array.init newsize (fun i ->
            if i < offset then !binary_operands.(i)
            else if i = offset then {f_binary=fun op l r -> T.binary_operand op (Obj.obj l) (Obj.obj r)}
            else uninit_binary
          )
        end end;
      offset

    let set t v =
      let v = Obj.repr v in
      let len = Array.length !t in
      if offset < len
      then Array.unsafe_set !t offset v
      (* Array is too small, create an extended copy *)
      else t := Array.init (offset+1) (fun i ->
        if i < len then Array.unsafe_get !t i
        else if i == offset then v
        else Array.unsafe_get !defaults i
      )


    let get t = get offset t |> Obj.obj

    let update' value = function
      | FromNil x -> FromNil { max=max x.max offset; update=(offset,Obj.repr value)::x.update }
      | FromPrev x -> FromPrev { x with max=max x.max offset; update=(offset,Obj.repr value)::x.update }

    let single_update x value = update x |> update' value |> finish
    let update = update'
  end

  module Field = MutableField
end

module NoOperands = struct
  type 'a unary_operand = 'a empty
  let init_unary (x: 'a unary_operand) = match x with _ -> .
  let combine_unary (x: 'a unary_operand) _ _ = match x with _ -> .

  type 'a binary_operand = 'a empty
  let init_binary (x: 'a binary_operand) = match x with _ -> .
  let combine_binary (x: 'a binary_operand) _ _ = match x with _ -> .
end

module NoFieldOperand = struct
  let unary_operand: 'a empty -> 'b -> 'a = function _ -> .
  let binary_operand: 'a empty -> 'b -> 'b -> 'a = function _ -> .
end
