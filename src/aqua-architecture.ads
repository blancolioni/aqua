with Aqua.Memory;

package Aqua.Architecture is

   type Register_Index is new Octet;

   type Aqua_Instruction is
     (A_TRAP,
      A_MUL, A_MULI, A_MULU, A_MULUI, A_DIV, A_DIVI, A_DIVU, A_DIVUI,
      A_ADD, A_ADDI, A_ADDU, A_ADDUI, A_SUB, A_SUBI, A_SUBU, A_SUBUI,
      A_2ADDU, A_2ADDUI, A_4ADDU, A_4ADDUI,
      A_8ADDU, A_8ADDUI, A_16ADDU, A_16ADDUI,
      A_CMP, A_CMPI, A_CMPU, A_CMPUI,
      A_NEG, A_NEGI, A_NEGU, A_NEGUI,
      A_SL, A_SLI, A_SLU, A_SLUI,
      A_SR, A_SRI, A_SRU, A_SRUI,
      A_LDB, A_LDBI, A_LDBU, A_LDBUI,
      A_LDH, A_LDHI, A_LDHU, A_LDHUI,
      A_LDW, A_LDWI,
      A_STB, A_STBI, A_STBU, A_STBUI,
      A_STH, A_STHI, A_STHU, A_STHUI,
      A_STW, A_STWI,
      A_JMP, A_JMPB, A_PUSHJ, A_PUSHJB, A_GETA, A_GETAB, A_PUT, A_PUTI
     );

   function To_Opcode (Instruction : Aqua_Instruction) return Octet;
   function To_Instruction (Opcode : Octet) return Aqua_Instruction;

   procedure Decode
     (Value       : Word;
      Instruction : out Aqua_Instruction;
      X, Y, Z     : out Octet);

end Aqua.Architecture;
