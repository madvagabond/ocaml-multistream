open Lwt.Infix 

let header = "/multistream/1.0.0"

module type IO = Io.S



module type S = sig
  type io

  type handler = (string * io -> unit Lwt.t)
  type error
  
  val select: io -> string list -> (unit, error) result Lwt.t
  val handle: io -> handler list -> (unit, error) result Lwt.t
  val ls: io -> (string list, error) result Lwt.t


  
  
end





module Make (IO: IO) = struct

  module U64 = Io.U64
  open IO
  type io = IO.t

  type error = [`IO_error of IO.error | `Protocol_mismatch | `Na | `Codec_error]
  type handler = (string * io -> unit Lwt.t)


  module LP = Io.LengthPrefix(IO)


  let select io proto =
    let payload = Cstruct.append (LP.encode header) (LP.encode proto) in

    let send = IO.write io payload |> Lwt_result.map_err
                                        (fun e -> `IO_error e)
    in
    
 
    send >>= fun s -> 
    LP.check io header >>= fun hdr -> 
    LP.check io proto >|= fun selected -> 

    if Rresult.R.is_error s then
      s
    else if Rresult.R.is_error hdr then
      hdr
    else
      selected




  
  let ls io =
    let open Lwt_result.Infix in

    let payload = Cstruct.concat [LP.encode header; LP.encode "ls"] in
    


    
    IO.write io payload |> Lwt_result.map_err (
                            fun e -> `IO_error e
                          )  >>= fun _ ->

    LP.check io header >>= fun _ ->
    LP.read_frame io >|= fun out ->

    LP.decode_protocols out
    

  


 

  
  

end
