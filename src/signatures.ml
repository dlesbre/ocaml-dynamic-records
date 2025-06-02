
(** The type of a record field.

    This module type is defined outside of {!S} in order to allow
    exporting individual record fields without having to export the whole record
    interface. *)
module type FIELD = sig
  type record
  (** The record type: {!S.t} *)

  type update
  (** The record's update type: see {!S.type-update} *)

  type t
  (** The field type, a part of the {!record} *)

  val get: record -> t
  (** Get the field's value *)

  val update: t -> update -> update
  (** Modify this field as part of an update. See {!S.type-update}. *)

  val single_update: record -> t -> record
  (** Shorthand to create a variant of a record wich only modifies this field.
      [single_update record value] is equivalent to
      [MyRecord.update record |> MyField.update value |> MyRecord.finish] *)

  val init: t -> record
  (** [init value] creates a new record where all fields have their {{!Dynamic_record.S.FIELD_PARAMETER.default}[default]} value except this
      one, which is set to [value].

      This is equivalent to [MyRecord.init |> MyField.update value |> MyRecord.finish] *)
end

(** The type of a record mutable field. An extension of {!FIELD}. *)
module type MUTABLE_FIELD = sig
  include FIELD

  val set: record -> t -> unit
  (** Set the field's value *)
end

module type OPERANDS = sig
  type 'a unary_operand
  (** A type reprensenting the record-wide unary operands
      (functions with type [t -> 'a]), The type parameter ['r] is [t],
      it is passed as a parameter since it isn't defined yet.

      For example, a print formatter and a clone operator
      {[
        type 'a unary_operand =
          | ToString : string unary_operand
          | Format : Format.formatter -> unit unary_operand (** Extra arguments can be embedded in the constructor. *)
          | Clone : t unary_operand (** Requires recursive modules to acces the record type t *)
      ]}

      Operands
  *)

  val init_unary: 'a unary_operand -> 'a
  (** [init_unary op] should return an initial value for the given operand.
      This is also the value this operand would have on the empty record (one with no fields). *)

  type 'a binary_operand
  (** Same as {!unary_operand}, but for binary functions.
      For example, [equal] and [compare]:
      {[
        type ('r, 'a) binary_operand =
          | Equal : ('r, bool) binary_operand
          | Compare : ('r, int) binary_operand
      ]}
  *)

  val init_binary: 'a binary_operand -> 'a
  (** Same as {!init_unary}, but for {!binary_operand}. *)
end

(** The type of a dynamic record.

    @canonical Dynamic_record.S *)
module type S = sig

  (** {1 Types} *)

  type t
  (** The record type.

      {b Warning:} using polymorphic [=], [compare] or [Hashtbl.hash] directly
      on records can lead to errors. Some records will be considered distinct
      despite having all field values equal. Instead use the provided {!equal},
      {!compare} and {!hash} functions, or better yet, implement them as operands.
  *)

  type 'a unary_operand
  (** A GADT representing possible unary operations on the record *)

  type 'a binary_operand
  (** A GADT representing possible binary operations on the record *)

  type update
  (** An update represents a set of changes of a record.
      It is used as an intermediate type so that multiple fields can be changed
      in one copy:
      - Use {!init} or {!val-update} to generate an empty update
        (either from the empty record or an existing one),
      - Define the field changes using the relevant {!Field.val-update} functions
      - When done, generate the new record with {!finish}.

      Note that in this pipeline, all computation/copies happens in the {!finish}
      function. So if a field is mutated between a call to {!init}/{!val-update}
      and one to {!finish}, the newly mutated value will be used. *)

  (** {1 Record creation and modification} *)

  val init: update
  (** Create an update from the empty record, aka the record where all fields are
      set to their {!FIELD_PARAMETER.default} values. *)

  val update: t -> update
  (** Create an update from an existing record *)

  val finish: update -> t
  (** Turn a set of updates into a new record. This function does the actual
      copying/memory allocation. *)

  (** {1 Creating fields} *)
  (** Fields are created by calling the {!Field} or {!MutableField} functors,
      or, equivalently, the {!val-field} or {!val-mutable_field} functions.
      Field do not have an explicit name at runtime. They are named by assigning
      the functor result to a module:
      {[
        module MyFieldName = Record.Field(Type)
        let my_field = Record.field typ
      ]} *)

  (** {2 Functorial interface} *)
  (** These functors allow creating fields as modules. *)

  (** The information required to initialize a field *)
  module type FIELD_PARAMETER = sig
    type t
    (** The type of the new field *)

    val default: t
    (** A default value used to initialize the new field *)

    val unary_operand: 'a unary_operand -> 'a -> t -> 'a
    (** Specify how each unary_operand acts on this field *)

    val binary_operand: 'a binary_operand -> 'a -> t -> t -> 'a
    (** Specify how each binary_operand acts on this field *)
  end

  (** Define a new record field with the given type and default value, equivalent to the {!val-field} function. *)
  module Field(T: FIELD_PARAMETER)() : FIELD
    with type t = T.t
     and type record = t
     and type update = update

  (** A new record field that can be mutated, equivalent to the {!val-mutable_field} function. *)
  module MutableField(T: FIELD_PARAMETER)() : MUTABLE_FIELD
    with type t = T.t
     and type record = t
     and type update = update

  (** {2 Functional interface} *)
  (** Fields can also be created using functions and records. This is equivalent
      to the functorial interface, but can be more lightweight when used in a
      function body, as it avoids using {{: https://dev.realworldocaml.org/first-class-modules.html}first class modules}. *)

  (** A record regrouping the information to create a field of type ['a].
      This is equivalent to {{!FIELD_PARAMETER}[FIELD_PARAMETER with type t = 'a]}
      in the functorial interface.*)
  type 'a field_parameter = {
    default: 'a; (** See {!FIELD_PARAMETER.default} *)
    unary_operand: 'b. 'b unary_operand -> 'b -> 'a -> 'b; (** See {!FIELD_PARAMETER.unary_operand} *)
    binary_operand: 'b. 'b binary_operand -> 'b -> 'a -> 'a -> 'b; (** See {!FIELD_PARAMETER.binary_operand} *)
  }

  (** A field value, this is equivalent to the {!FIELD} module type in the
      modular interface *)
  type 'a field = {
    get: t -> 'a; (** See {!FIELD.get} *)
    update: 'a -> update -> update; (** See {!FIELD.val-update} *)
    single_update: t -> 'a -> t;  (** See {!FIELD.single_update} *)
  }

  (** A mutable field value, equivalent to the {!MUTABLE_FIELD} module type in the
      modular interface*)
  type 'a mutable_field = {
    get: t -> 'a; (** See {!MUTABLE_FIELD.get} *)
    update: 'a -> update -> update; (** See {!MUTABLE_FIELD.val-update} *)
    single_update: t -> 'a -> t;  (** See {!MUTABLE_FIELD.single_update} *)
    set: t -> 'a -> unit; (** See {!MUTABLE_FIELD.set} *)
  }

  (** Define a new field with the given type and default value, equivalent to the {!Field} functor. *)
  val field: 'a field_parameter -> 'a field

  (** Defina a new mutable field with the given type and default value, equivalent to the {!MutableField} functor. * *)
  val mutable_field: 'a field_parameter -> 'a mutable_field

  val field_of_mutable_field: 'a mutable_field -> 'a field
  (** Cast a {{!type-mutable_field}['a mutable_field]} into a {{!type-field}['a field]},
      forgets that the field is mutable. *)

  (** {1 Record-wide operations} *)

  val copy: t -> t
  (** [copy record] creates a shallow copy of [record].
      Modifying a {!MutableField} on one will not reflect on the other.

      However, the field values are not deep copied. Therefore,
      if the field contents are mutable (e.g. a field whose type is [_ ref]),
      then modifying it will affect both vectors. To avoid this issue,
      you should implement a [clone] operand. *)

  val equal: t -> t -> bool
  (** Equality test on records, uses polymorphic equality [=] to compare fields. For a more robust
      version specifying each field's [equal] function, implement an [equal] operand.

      {b Warning:} using polymorphic [=] directly on records can lead to errors, i.e. some
      records will be considered distinct despite having all field values equal.

      Execution time is linear in the number of fields (with an early-return). *)

  val compare: t -> t -> int
  (** Comparison test on records, uses polymorphic [compare] to compare fields. For a more robust
      version specifying each field's [compare] function, implement an [compare] operand.

      {b Warning:} using polymorphic [compare] directly on records can lead to errors, i.e. some
      records will be considered distinct despite having all field values equal.

      Execution time is linear in the number of fields (with an early-return). *)

  val hash: t -> int
  (** Hash a record, using polymorphic [Hashtbl.hash] to hash fields. For a more robust
      version specifying each field's [hash] function, implement an [hash] operand.

      {b Warning:} using polymorphic [Hashtbl.hash] directly on records can lead to errors, i.e. some
      records will be considered distinct despite having all field values equal.

      Execution time is linear in the number of fields. *)

  val unary_operand: 'a unary_operand -> t -> 'a
  (** Compute a unary_operation on the whole record.
      *)

  val binary_operand: 'a binary_operand -> t -> t -> 'a
end

type empty = |
(** An empty type, used to implement empty operands. *)
