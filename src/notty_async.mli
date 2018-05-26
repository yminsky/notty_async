open! Base
open! Async

module Term : sig
  type t

  val create
    :  ?dispose:bool
    -> ?nosig:bool
    -> ?mouse:bool
    -> ?bpaste:bool
    -> ?reader:Reader.t (** stdin by default *)
    -> ?writer:Writer.t (** stdout by default *)
    -> ?on_resize: (int * int -> unit)
    (** Called at least once after every resize. *)
    -> on_input:(Notty.Unescape.event -> unit)
    (** Called for every event received by the terminal *)
    -> stop:unit Deferred.t
    -> unit
    -> t Deferred.t

  val refresh     : t -> unit Deferred.t
  val write_image : t -> Notty.image -> unit Deferred.t
  val set_cursor  : t -> (int * int) option -> unit Deferred.t
  val size        : t -> int * int
end
