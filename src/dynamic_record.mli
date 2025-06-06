include module type of Signatures

(**/**)
(** Prelude for mdx
{[
  open Dynamic_record
]} *)
(**/**)

(** Create a dynamic record with the given operands.
    For the simplest version without operands, call it with the {!NoOperands}
    module as argument. *)
module Make(Operands: OPERANDS)() : S
  with type 'a unary_operand = 'a Operands.unary_operand
   and type 'a binary_operand = 'a Operands.binary_operand

(** An operand specification for empty operands (no record wide unary or binary functions).
    Used as argument to {!Make}. *)
module NoOperands : OPERANDS
  with type 'a unary_operand = empty
   and type 'a binary_operand = empty

(** Use this module in the functor arguments of {!Make.Field} or {!Make.MutableField}
    when not using any operands (passing {!NoOperands} to {!Make}).

    For example:
    {[
      module Record = Make(Dynamic_record.NoOperands)()

      module IntField = Record.Field(struct
        type t = int
        let default = Value (-1)
        include NoFieldOperands
      end)
    ]} *)
module NoFieldOperands : sig
  val unary_operand: empty -> 'a -> 'b -> 'a
  val binary_operand: empty -> 'a -> 'b -> 'b -> 'a
end
