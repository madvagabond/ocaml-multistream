open Lwt.Infix 

let header = "/multistream/1.0.0"

module type IO = Io.S



module type S = sig
  type io

  type handler = string * (io -> unit Lwt.t)
  type error
  
  val select: io -> string -> (unit, error) result Lwt.t
  val handle: io -> handler list -> (unit, error) result Lwt.t
  val ls: io -> (string list, error) result Lwt.t


  
  
end





module Make (IO: IO) = struct

  module U64 = Io.U64
  
  type io = IO.t

  type error = [`IO_error of IO.error | `Protocol_mismatch | `Na | `Codec_error]
  type handler = string * (io -> unit Lwt.t)


  module LP = Io.LengthPrefix(IO)


  let select io proto =
    let payload = Cstruct.append
                    (LP.encode_string header)
                    (LP.encode_string proto)
    in

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

    let payload = Cstruct.concat [
                      LP.encode_string header;
                      LP.encode_string "ls"
                    ]
    in
    


    
    IO.write io payload |> Lwt_result.map_err (
                            fun e -> `IO_error e
                          )  >>= fun _ ->

    LP.check io header >>= fun _ ->
    LP.read_frame io >|= fun out ->

    LP.decode_protocols out
    

  



  
  let handle_ls io protos =

    let payload =
      List.map (fun x -> LP.encode_string x) protos
      |> Cstruct.concat
      |> LP.encode
    in
    
    IO.write io payload |> LP.io_error 




  
  let handle_select io handlers proto =
    let open Lwt_result.Infix in
    
    let route = List.find_opt (fun (rt, _) -> rt = proto) handlers in
    
    match route with
    | Some (_, cb) ->

      let pl = LP.encode_string proto in
      
      IO.write io pl |> LP.io_error >>= fun _ ->
      cb io |> Lwt.map (fun () ->
                    let opt = Some () in
                    Ok opt
                  ) 
  
    | None ->

       let na = LP.encode_string "na" in 
       IO.write io na |> LP.io_error >|= fun _ ->
       None       



  let rec handle io handlers =
    let open Lwt_result.Infix in
    
    let header_p = LP.encode_string header in
    IO.write io header_p  |> LP.io_error >>= fun _ -> 
    LP.check io header >>= fun () ->
    LP.read_frame io >>= fun proto ->

    let protocol = Cstruct.to_string proto in

    match protocol with

    | "ls" ->
       let supported = List.map (fun (a, _) -> a ) handlers in 
       handle_ls io supported >>= fun _ ->
       handle io handlers 

    | _ -> 
       handle_select io handlers protocol >>= fun res ->
       (
         match res with
         | Some _ -> Lwt_result.return ()
         | None -> handle io handlers 
       )




  
    
end
