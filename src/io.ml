module type S = sig

  type t 
  type error
  
  val read: t -> (Cstruct.t, error) result Lwt.t
  val write: t -> Cstruct.t -> (unit, error) result Lwt.t
  
  val read_exact: t -> int -> (Cstruct.t, error) result Lwt.t
  val read_some: t -> int -> (Cstruct.t, error) result Lwt.t

  val close: t -> unit Lwt.t

  
end




module U64 = Varint.Make(Stdint.Uint64)




module LengthPrefix (IO: S) = struct
  type error = [`IO_error of IO.error | `Protocol_mismatch | `Na | `Codec_error]

  let io_error res =
    Lwt_result.map_err (fun e -> `IO_error e) res 



  

  let encode token =


    let len = Stdint.Uint64.of_int (Cstruct.len token) in

    Cstruct.concat [
        U64.to_cstruct len;
        token; 
        Cstruct.of_string "\n"
      ]


  let encode_string s =
    Cstruct.of_string s |> encode


  let read_frame io = 
    let open Lwt_result.Infix in 

      let read = IO.read_some io 10 |> io_error in


      let lp =
        Lwt_result.bind_result read (fun buf ->
            U64.of_cstruct buf
            |> Rresult.R.reword_error (fun _ -> `Codec_error)
          )
      in 


      
      

      lp >>= fun (vi, rest) ->
      
      let len = Stdint.Uint64.to_int vi in

      let need = len - (Cstruct.len rest) in

      if need > 0 then
        IO.read_exact io need
        |> io_error >|= fun buf ->
        let d = Cstruct.append rest buf in
        let (a, _) = Cstruct.split d (len - 1) in
        a

      else
        let (out, _) = Cstruct.split rest (len - 1) in
        Ok out |> Lwt.return
  



  let check io proto =
    let open Lwt_result.Infix in
    
      read_frame io >>= fun tok ->
      let token = Cstruct.to_string tok in
  
      if proto = "na" then Lwt_result.fail `Na
  
      else if token = proto then Lwt_result.return ()

      else Lwt_result.fail `Protocol_mismatch



  let decode_protocols buf =

    let rec aux protos buf =

      if Cstruct.len buf > 0 then
        let dec = U64.of_cstruct buf in
        let (vi, rest) = Rresult.R.get_ok dec in
        let len = Stdint.Uint64.to_int vi in
        
        let (a, b) = Cstruct.split rest len in
        let protos = protos @ [Cstruct.to_string a] in
        aux protos b

      else
        protos 
    in
    
    aux [] buf
  


end































               
