module type S = sig 
  type t 
  val instr_to_string: t -> string 
  val string_to_instr: string -> t 
  val line_parse: string -> t 
end 