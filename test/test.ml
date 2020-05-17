module IO = struct


  open Lwt.Infix
         
         
  module C = Mirage_channel.Make(Mirage_flow_unix.Fd)

  
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



module MS = Multistream.Make(IO)



let create_conn () =

  let (client, server) = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM) 0 in 
  IO.create client, IO.create server





let do_nothing _flow =
  let _ = print_endline "nothing" in
  Lwt.return_unit





let print_error =
  function
  | Ok () -> print_endline "ok"
  | Error `IO_error _ -> failwith "io_error"
  | Error `Codec_error -> failwith "codec error"
  | Error `Protocol_mismatch -> failwith "protocol mismatch"
  | Error `Na -> failwith "na"



let select_test _s () =
  
  let open Lwt.Infix in 
  let cli, srv = create_conn () in
  let handle = MS.handle srv [("/nothing", do_nothing) ] in
  let _ = Lwt.async (fun () -> handle >|= print_error) in 

  MS.select cli "/nothing" >|= print_error




let tests = [
  Alcotest_lwt.test_case "protocol negotiation" `Quick select_test
]

let _ =  Lwt_main.run @@ Alcotest_lwt.run "Multistream" [
    "mss-select", tests 
  ]
