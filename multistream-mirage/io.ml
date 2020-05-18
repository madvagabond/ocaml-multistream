module Make (S: Mirage_flow.S) = struct
  

  open Lwt.Infix
         
         
  module C = Mirage_channel.Make(S)

  
  type error = [
      `Read_error of C.error | `Eof | `Write_error of C.write_error
  ]

  
  type t = C.t



  let eof_to_err = function 
    | Ok `Data buf -> Ok buf
    | Ok `Eof -> Error `Eof
    | Error e -> Error (`Read_error e)
  
 
  let read_some t len =
    C.read_some t ~len >|= eof_to_err

  
  let read_exact t len =
    C.read_exactly t ~len >|= fun x ->
    let y = eof_to_err x in
    Result.map Cstruct.concat y


  let write t buf =
    let _ = C.write_buffer t buf in

    Lwt_result.map_err
      (fun e -> `Write_error e)
      (C.flush t) 


  let close t =
    C.close t >|= fun _ -> ()

  let read t = C.read_some t ~len:1024 >|= eof_to_err

  let create = C.create

end 
