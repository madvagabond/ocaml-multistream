module Make: functor (S: Mirage_flow.S) ->  sig

  module C: Mirage_channel.S

  
  include Multistream.IO with type error = [
      `Read_error of C.error | `Eof | `Write_error of C.write_error
  ] and type t = C.t 

  
  val create: S.flow -> t 
  
end
