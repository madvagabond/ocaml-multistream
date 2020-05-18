
module type IO = Io.S





module type S = sig
  type io

  type handler = string * (io -> unit Lwt.t)
  type error
  
  val select: io -> string -> (unit, error) result Lwt.t
  val handle: io -> handler list -> (unit, error) result Lwt.t
  val ls: io -> (string list, error) result Lwt.t


  
  
end

module Make: functor (Io: IO) -> S with type error =  [`IO_error of Io.error | `Protocol_mismatch | `Na | `Codec_error] and type io = Io.t
