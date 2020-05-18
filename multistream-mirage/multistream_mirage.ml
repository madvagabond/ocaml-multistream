module IO = Io


module Make (S: Mirage_flow.S) = struct

  module IO = IO.Make(S)      
  include Multistream.Make(IO)

end
