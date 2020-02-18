module Riscv : Isa.S = struct 

  (* All the RISC-V instructions *)
  type r_type = Add | Sub | Sll | Slt | Sltu | Xor | Srl | Sra | Or | And
  type i_type = Addi | Slti | Sltiu | Xori | Ori | Andi 
  type s_type = Sw | Sh | Sd | Lb | Lw | Ld | Lbu | Lhu
  type b_type = Beq  | Bne | Blt | Bge | Bltu | Bgeu
  type u_type = Lui
  type j_type = Jal | Jalr

  (* type pure_instr = R of r_type | I of i_type | S of s_type | B of b_type | U of u_type | J of j_type *)
  type pseudo = Sext_w | Misc of string | Nomatch
  type t = R of r_type | I of i_type | S of s_type | B of b_type | U of u_type | J of j_type | P of pseudo | C of t 

  (* type r_type_instr = r_type * reg * reg * reg
  type i_type_instr = i_type * reg * reg * imm
  type s_type_instr = s_type * reg * offset * reg
  type b_type_instr = b_type * reg
  and reg = string 
  and offset = int
  and imm = int  *)

  let rec instr_to_string : t -> string = function
    | R Add -> "add"
    | S Lb   -> "lb"
    | I Addi -> "addi"
    | B Beq  -> "beq"
    | S Lw   -> "lw"
    | B Bltu -> "bltu"
    | J Jalr -> "jalr"
    | B Bge  -> "bge"
    | I Ori  -> "ori"
    | R Sltu -> "sltu"
    | R Xor  -> "xor"
    | S Lhu  -> "lhu"
    | R Sll  -> "sll"
    | R Sub  -> "sub"
    | B Bne  -> "bne"
    | S Sd   -> "sd"
    | B Bgeu -> "bgeu"
    | I Sltiu -> "sltiu"
    | S Sh   -> "sh"
    | I Andi -> "andi"
    | I Xori -> "xori"
    | R Srl  -> "srl"
    | R Sra  -> "sra"
    | I Slti -> "slti"
    | S Ld   -> "ld"
    | S Lbu  -> "lbu"
    | J Jal  -> "jal"
    | U Lui  -> "lui"
    | R Or  -> "or"
    | R And  -> "and"
    | P Sext_w -> "sext.w"
    | B Blt  -> "blt"
    | S Sw   -> "sw"
    | R Slt  -> "slt"
    | C instr -> "c." ^ (instr_to_string instr)
    | P Misc s -> s
    | P Nomatch -> "no_match"

  let rec string_to_instr : string -> t = function
    | "add"  -> R Add 
    | "lb"   -> S Lb     
    | "addi" -> I Addi 
    | "beq"  -> B Beq  
    | "lw"   -> S Lw
    | "bltu" -> B Bltu
    | "jalr" -> J Jalr
    | "bge"  -> B Bge  
    | "ori"  -> I Ori  
    | "sltu" -> R Sltu
    | "xor"  -> R Xor  
    | "lhu"  -> S Lhu  
    | "sll"  -> R Sll  
    | "sub"  -> R Sub  
    | "bne"  -> B Bne  
    | "sd"   -> S Sd     
    | "bgeu" -> B Bgeu
    | "sltiu" -> I Sltiu
    | "sh"   -> S Sh     
    | "andi" -> I Andi
    | "xori" -> I Xori
    | "srl"  -> R Srl  
    | "sra"  -> R Sra  
    | "slti" -> I Slti
    | "ld"   -> S Ld     
    | "lbu"  -> S Lbu  
    | "jal"  -> J Jal  
    | "lui"  -> U Lui  
    | "or"   -> R Or     
    | "and"  -> R And  
    | "sext.w" -> P Sext_w
    | "blt"  -> B Blt  
    | "sw"   -> S Sw    
    | "slt"  -> R Slt
    | s      ->  
      let arr = Array.of_list (String.split_on_char '.' s) in 
        if Array.length arr = 2 then (C (string_to_instr arr.(1)))
        else P (Misc s)

  (* Parsing a line of the log *)
  let line_parse (line : string) = 
    let arr = Array.of_list (String.split_on_char ' ' line) in 
    if Array.length arr >= 7 then 
      (string_to_instr arr.(6))
    else 
      P Nomatch
end 