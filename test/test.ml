



module MS = Multistream_unix

module IO = MS.IO


let create_conn () =

  let (client, server) = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM) 0 in 
  IO.create client, IO.create server





let do_nothing _flow =
  let _ = print_endline "nothing" in
  Lwt.return_unit





let print_error =
  function
  | Ok _ -> print_endline "ok"
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


let test_ls _s () =
  let open Lwt.Infix in

  let cli, srv = create_conn () in
  let protos = ["/nothing"] in
  let handler =  MS.handle srv [("/nothing", do_nothing) ] in
  let _ = Lwt.async (fun () -> handler >|= print_error) in

  MS.ls cli >|= fun res -> 
  let _ = print_error res in
  let supported = Result.get_ok res in
  Alcotest.(check (list string)) "same protocols" protos supported 
  


let tests = [
  Alcotest_lwt.test_case "protocol negotiation" `Quick select_test;
  Alcotest_lwt.test_case "ls" `Quick test_ls  
]




let _ =  Lwt_main.run @@ Alcotest_lwt.run "Multistream" [
    "mss-select", tests 
  ]

