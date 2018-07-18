package body Aqua.Architecture is

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Value       : Word;
      Instruction : out Aqua_Instruction;
      X, Y, Z     : out Octet)
   is
      It : Word := Value;
   begin
      Z := Octet (It mod 256);
      It := It / 256;
      Y := Octet (It mod 256);
      It := It / 256;
      X := Octet (It mod 256);
      It := It / 256;
      Instruction := To_Instruction (Octet (It));
   end Decode;

   --------------------
   -- To_Instruction --
   --------------------

   function To_Instruction (Opcode : Octet) return Aqua_Instruction
                            is separate;
   ---------------
   -- To_Opcode --
   ---------------

   function To_Opcode (Instruction : Aqua_Instruction) return Octet is
   begin
      case Instruction is
         when A_TRAP =>
            return 0;
         when A_MUL .. A_SRUI =>
            return 16#18# + Aqua_Instruction'Pos (Instruction)
              - Aqua_Instruction'Pos (A_MUL);
         when A_LDB .. A_LDWI =>
            return 16#80# + Aqua_Instruction'Pos (Instruction)
              - Aqua_Instruction'Pos (A_LDB);
         when A_STB .. A_STWI =>
            return 16#A0# + Aqua_Instruction'Pos (Instruction)
              - Aqua_Instruction'Pos (A_STB);
      end case;
   end To_Opcode;

end Aqua.Architecture;
