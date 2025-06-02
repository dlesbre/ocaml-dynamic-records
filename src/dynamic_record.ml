
include Signatures

let error msg = failwith ("dynamic-records internal error: " ^ msg)

module type FIELD_VALUE = sig
  type fields
  module NewField(T: sig type t end) : sig
    val value_of_fields: fields -> T.t
    val fields_of_value: T.t -> fields
  end
end

module Make(O: OPERANDS)() = struct

  module TypeSafe : FIELD_VALUE = struct
    type fields = ..

    module NewField(T: sig type t end) = struct
      type fields += F of T.t
      let value_of_fields = function
        | F x -> x
        | _ -> error "type mismatch in fields"

      let fields_of_value t = F t
    end
  end

  (* The type information is already encoded in each fields position,
     therefore we can forget it. This unsafe version is slightly faster as it
     avoids an indirection and a dynamic check. *)
  module TypeUnsafe : FIELD_VALUE = struct
    type fields = Obj.t

    module NewField(T: sig type t end) = struct
      let value_of_fields: fields -> T.t = Obj.obj
      let fields_of_value = Obj.repr
    end
  end

  include TypeUnsafe

  type t = fields array ref
  type 'a unary_operand = 'a O.unary_operand
  type 'a binary_operand = 'a O.binary_operand

  type unary_wrapper = { f_unary: 'a. 'a unary_operand -> 'a -> fields -> 'a } [@@unboxed]
  type binary_wrapper = { f_binary: 'a. 'a binary_operand -> 'a -> fields -> fields -> 'a } [@@unboxed]

  (** A value for extra fields *)
  type defaults =
    | DValue of fields
    | DInit of (unit -> fields)
    | DUninitialized
  let uninit_default = DUninitialized
  let uninit_unary = { f_unary=fun _ _ -> error "Uninitilized unary operand"}
  let uninit_binary = { f_binary=fun _ _ _ -> error "Uninitilized unary operand"}

  let initial_size = 5

  let value_of_default = function
    | DValue x -> x
    | DInit f -> f ()
    | DUninitialized -> error "Uninitialized default value"

  (* INVARIANT:
    - all three of these arrays always have the same size,
    - that size is greater than the number of fields [size].
    - they only have uninit_XXX values at positions greater than the number of fields [size]. *)
  let defaults = ref (Array.make initial_size uninit_default)
  let unary_operands = ref (Array.make initial_size uninit_unary)
  let binary_operands = ref (Array.make initial_size uninit_binary)
  let size = ref 0

  type update =
    | FromNil of { max:int; update: (int * fields) list}
    | FromPrev of { max:int; update: (int * fields) list; prev: t }

  let init = FromNil { max=0; update=[] }
  let update t = FromPrev { max = Array.length !t - 1; update = []; prev = t }

  let finish x =
    let array = match x with
    | FromNil {max;update} -> Array.init (max+1) (fun i ->
        match List.assoc_opt i update with
        | Some v -> v
        | None -> value_of_default !defaults.(i))
    | FromPrev {max;update;prev} -> Array.init (max+1) (fun i ->
        match List.assoc_opt i update with
        | Some v -> v
        | None -> if i < Array.length !prev
                  then !prev.(i)
                  else value_of_default !defaults.(i))
    in ref array

  let copy x = ref (Array.copy !x)

  let get offset t =
    if offset < Array.length !t
    then Array.unsafe_get !t offset
    else value_of_default !defaults.(offset)

  let rec unary_operand op r i acc =
    if i >= !size then acc
    else
      get i r |>
      !unary_operands.(i).f_unary op acc |>
      unary_operand op r (i+1)

  let unary_operand op r = unary_operand op r 0 (O.init_unary op)

  let rec binary_operand op l r i acc =
    if i >= !size then acc
    else
      let vl = get i l in
      let vr = get i r in
      !binary_operands.(i).f_binary op acc vl vr |>
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

  module type FIELD_PARAMETER = sig
    type t
    val default: t default
    val unary_operand: 'a unary_operand -> 'a -> t -> 'a
    val binary_operand: 'a binary_operand -> 'a -> t -> t -> 'a
  end

  module MutableField(T: FIELD_PARAMETER)() = struct
    type record = t
    type nonrec update = update
    type t = T.t

    module F = NewField(T)

    let defaults_of_default = function
      | Value x -> DValue (F.fields_of_value x)
      | Initializer f -> DInit (fun () -> F.fields_of_value (f ()))

    let offset =
      let offset = !size in
      incr size;
      begin if Array.length !defaults >= !size
        then begin
          !defaults.(offset) <- defaults_of_default T.default;
          !unary_operands.(offset) <- {f_unary=fun op acc r -> T.unary_operand op acc (F.value_of_fields r)};
          !binary_operands.(offset) <- {f_binary=fun op acc l r -> T.binary_operand op acc (F.value_of_fields l) (F.value_of_fields r)};
        end else begin
          let newsize = 2 * !size in
          defaults := Array.init newsize (fun i ->
            if i < offset then !defaults.(i)
            else if i = offset then defaults_of_default T.default
            else uninit_default
          );
          unary_operands := Array.init newsize (fun i ->
            if i < offset then !unary_operands.(i)
            else if i = offset then {f_unary=fun op acc r -> T.unary_operand op acc (F.value_of_fields r)}
            else uninit_unary
          );
          binary_operands := Array.init newsize (fun i ->
            if i < offset then !binary_operands.(i)
            else if i = offset then {f_binary=fun op acc l r -> T.binary_operand op acc (F.value_of_fields l) (F.value_of_fields r)}
            else uninit_binary
          )
        end end;
      offset

    let set t v =
      let v = F.fields_of_value v in
      let len = Array.length !t in
      if offset < len
      then Array.unsafe_set !t offset v
      (* Array is too small, create an extended copy *)
      else t := Array.init (offset+1) (fun i ->
        if i < len then Array.unsafe_get !t i
        else if i == offset then v
        else value_of_default (Array.unsafe_get !defaults i)
      )


    let get t = get offset t |> F.value_of_fields

    let update' value = function
      | FromNil x -> FromNil { max=max x.max offset; update=(offset,F.fields_of_value value)::x.update }
      | FromPrev x -> FromPrev { x with max=max x.max offset; update=(offset,F.fields_of_value value)::x.update }

    let single_update x value = update x |> update' value |> finish
    let update = update'
    let init value = init |> update value |> finish
  end

  module Field = MutableField

  type 'a field_parameter = {
    default: 'a default;
    unary_operand: 'b. 'b unary_operand -> 'b -> 'a -> 'b;
    binary_operand: 'b. 'b binary_operand -> 'b -> 'a -> 'a -> 'b;
  }


  type 'a field = {
    get: t -> 'a;
    update: 'a -> update -> update;
    single_update: t -> 'a -> t;
  }

  type 'a mutable_field = {
    get: t -> 'a;
    update: 'a -> update -> update;
    single_update: t -> 'a -> t;
    set: t -> 'a -> unit;
  }

  let mutable_field (type a) (p: a field_parameter) : a mutable_field =
    let module Field = MutableField(struct
      type t = a
      let default = p.default
      let unary_operand = p.unary_operand
      let binary_operand = p.binary_operand
    end)() in {
      get = Field.get;
      update = Field.update;
      single_update = Field.single_update;
      set = Field.set;
    }

  let field_of_mutable_field (p: 'a mutable_field) = {
    get = p.get;
    update = p.update;
    single_update = p.single_update;
  }

  let field p = field_of_mutable_field (mutable_field p)
end

module NoOperands = struct
  type 'a unary_operand = empty
  let init_unary (x: 'a unary_operand) = match x with _ -> .

  type 'a binary_operand = empty
  let init_binary (x: 'a binary_operand) = match x with _ -> .
end

module NoFieldOperands = struct
  let unary_operand: empty -> 'a -> 'b -> 'a = function _ -> .
  let binary_operand: empty -> 'a -> 'b -> 'b -> 'a = function _ -> .
end
