module type S = sig 
  type t 
    (** The instruction type *)

  val instr_to_string: t -> string 
    (** Taking an instruction and converting to the assembly-name *)

  val string_to_instr: string -> t 
    (** Taking the assmebly-name and returning the correct instruction *)

  val line_parse: string -> t 
    (** A function that takes a line of dynamic execution and produces an instruction *)
end 