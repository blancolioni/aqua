with Ada.Text_IO;
with Aqua.IO;

package body Aqua.Architecture is

   ---------------------------
   -- Calculate_Instruction --
   ---------------------------

   function Calculate_Instruction
     (Instruction : Octet)
      return Aqua_Instruction
   is
      subtype Octet_2 is Octet range 0 .. 3;
      subtype Octet_4 is Octet range 0 .. 15;
      Size_Bits     : constant Octet_2 := Instruction / 64;
      Op_Count_Bits : constant Octet_2 := Instruction / 16 mod 4;
      Low_Nybble    : constant Octet_4 := Instruction mod 16;
   begin
      if Size_Bits = 0 then
         case Op_Count_Bits is
            when 0 =>
               declare
                  Result : constant No_Operand_Instruction :=
                             No_Operand_Instruction'Val (Instruction);
               begin
                  return Result;
               end;
            when 1 =>
               return A_Trap;
            when 2 =>
               return A_Get_Property;
            when 3 =>
               case Low_Nybble is
                  when 0 =>
                     return A_Set_Property;
                  when 1 =>
                     return A_Iterator_Start;
                  when 4 =>
                     return A_Jmp;
                  when 5 =>
                     return A_Jsr;
                  when 6 =>
                     return A_Goto;
                  when 8 .. 15 =>
                     return A_Call;
                  when others =>
                     raise Bad_Instruction with Octet'Image (Instruction);
               end case;
         end case;
      elsif Size_Bits = 1 and then Op_Count_Bits = 0 then
         declare
            Result : constant Branch_Instruction :=
                       Branch_Instruction'Val (Aqua_Instruction'Pos (A_Br)
                                               + Low_Nybble - 1);
         begin
            return Result;
         end;
      elsif Size_Bits = 2 and then Op_Count_Bits = 0 then
         return A_Iterator_Next;
      elsif Size_Bits = 3 and then Op_Count_Bits = 0 then
         declare
            Result : constant Float_Instruction :=
                       Float_Instruction'Val
                         (Aqua_Instruction'Pos (Float_Instruction'First)
                          + Low_Nybble / 2);
         begin
            return Result;
         end;
      else
         case Op_Count_Bits is
            when 0 =>
               raise Bad_Instruction with Octet'Image (Instruction);
            when 1 =>
               declare
                  Result : constant Single_Operand_Instruction :=
                             Single_Operand_Instruction'Val
                               (Aqua_Instruction'Pos
                                  (Single_Operand_Instruction'First)
                                + Low_Nybble);
               begin
                  return Result;
               end;
            when 2 =>
               declare
                  Result : constant Double_Operand_Instruction :=
                             Double_Operand_Instruction'Val
                               (Aqua_Instruction'Pos
                                  (Double_Operand_Instruction'First)
                                + Low_Nybble);
               begin
                  return Result;
               end;
            when 3 =>
               declare
                  Result : constant Triple_Operand_Instruction :=
                             Triple_Operand_Instruction'Val
                               (Aqua_Instruction'Pos
                                  (Triple_Operand_Instruction'First)
                                + Low_Nybble);
               begin
                  return Result;
               end;
         end case;
      end if;
   end Calculate_Instruction;

   ------------
   -- Encode --
   ------------

   function Encode
     (Instruction : Aqua_Instruction;
      Size        : Data_Size := Word_32_Size;
      Immediate   : Octet := 0)
      return Octet
   is
   begin
      case Instruction is
         when No_Operand_Instruction =>
            return Aqua_Instruction'Pos (Instruction);
         when Single_Operand_Instruction =>
            return (Data_Size'Pos (Size) + 1) * 64
              + 2#00010000#
            + (Aqua_Instruction'Pos (Instruction)
               - Aqua_Instruction'Pos (Single_Operand_Instruction'First));
         when Double_Operand_Instruction =>
            return (Data_Size'Pos (Size) + 1) * 64
              + 2#00100000#
            + (Aqua_Instruction'Pos (Instruction)
               - Aqua_Instruction'Pos (Double_Operand_Instruction'First));
         when Triple_Operand_Instruction =>
            return (Data_Size'Pos (Size) + 1) * 64
              + 2#0011_0000#
            + (Aqua_Instruction'Pos (Instruction)
               - Aqua_Instruction'Pos (Triple_Operand_Instruction'First));
         when Single_Operand_Float_Instruction =>
            return (if Size = Float_32_Size then 0 else 1)
              + 2#1100_1000#
            + Aqua_Instruction'Pos (Instruction)
              - Aqua_Instruction'Pos (Single_Operand_Float_Instruction'First);
         when Double_Operand_Float_Instruction =>
            return (if Size = Float_32_Size then 0 else 1)
              + 2#1100_0000#
            + Aqua_Instruction'Pos (Instruction)
              - Aqua_Instruction'Pos (Double_Operand_Float_Instruction'First);
         when Branch_Instruction =>
            return 2#0100_0001# +
              (Aqua_Instruction'Pos (Instruction)
               - Aqua_Instruction'Pos (Branch_Instruction'First));
         when A_Get_Property =>
            return 2#00100000# + Immediate mod 16;
         when A_Set_Property =>
            return 2#00110000#;
         when A_Iterator_Start =>
            return 2#00110001#;
         when A_Iterator_Next =>
            return 2#10000000# + Immediate mod 16;
         when A_Jmp =>
            return 2#00110100#;
         when A_Jsr =>
            return 2#00110101#;
         when A_Goto =>
            return 2#00110110#;
         when A_Call =>
            return 2#00111000# + Immediate mod 8;
         when A_Trap =>
            return 2#00010000# + Immediate mod 16;
      end case;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Operand : Operand_Type)
      return Octet
   is
   begin
      if Operand.Mode = Literal then
         return Operand.Lit;
      else
         return (Addressing_Mode'Pos (Operand.Mode)) * 32
           + Boolean'Pos (Operand.Deferred) * 16
           + Octet (Operand.Register);
      end if;
   end Encode;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class)
      return Address
   is
      Result : Address;
      Auto_Size : constant Word :=
                    (if Operand.Deferred
                     then Data_Octets (Address_Size)
                     else Data_Octets (Size));
   begin
      case Operand.Mode is
         when Literal =>
            raise Constraint_Error with
              "cannot get address of literal mode";
         when Register =>
            if Operand.Deferred then
               return Get_Address (R (Operand.Register));
            else
               raise Program_Error
                 with "cannot get address of register mode";
            end if;
         when Autoincrement =>
            Result := Get_Address (R (Operand.Register));
            R (Operand.Register) := R (Operand.Register) + Auto_Size;
         when Autodecrement =>
            R (Operand.Register) := R (Operand.Register) - Auto_Size;
            Result := Get_Address (R (Operand.Register));
         when Indexed | Indexed_8 | Indexed_16 =>
            Result := Get_Address (R (Operand.Register));
            declare
               Index_Size : constant Data_Size :=
                              (if Operand.Mode = Indexed
                               then Word_32_Size
                               elsif Operand.Mode = Indexed_16
                               then Word_16_Size
                               else Word_8_Size);
               A : constant Word :=
                              Memory.Get_Value (Get_Address (R (R_PC)),
                                                Index_Size);
            begin

               if Trace then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (A, Index_Size));
               end if;

               if Operand.Mode = Indexed_16 then
                  if A < 32768 then
                     Result := Result + Address (A);
                  else
                     Result := Result - Address (65536 - A);
                  end if;
               elsif Operand.Mode = Indexed_8 then
                  if A < 128 then
                     Result := Result + Address (A);
                  else
                     Result := Result - Address (256 - A);
                  end if;
               else
                  if Is_Integer (A) then
                     declare
                        I : constant Aqua_Integer := Get_Integer (A);
                     begin
                        if I < 0 then
                           Result := Result - Address (abs I);
                        else
                           Result := Result + Address (I);
                        end if;
                     end;
                  else
                     Result := Result + Get_Address (A);
                  end if;
               end if;

               R (R_PC) := R (R_PC) + Data_Octets (Index_Size);
            end;
      end case;

      if Operand.Deferred then
         Result := Get_Address (Memory.Get_Value (Result, Word_32_Size));
      end if;

      return Result;
   end Get_Address;

   ---------------------
   -- Get_Instruction --
   ---------------------

   function Get_Instruction
     (Instruction : Octet)
      return Aqua_Instruction
      is separate;

   -----------------
   -- Get_Operand --
   -----------------

   function Get_Operand
     (Op : Octet)
      return Operand_Type
   is
   begin
      if (Op and 2#11100000#) = 0 then
         return (Register => 0,
                 Deferred => False,
                 Mode     => Literal,
                 Lit      => Op);
      else
         return (Register => Register_Index (Op mod 16),
                 Deferred => (Op and 2#00010000#) /= 0,
                 Mode     => Addressing_Mode'Val (Op / 32),
                 Lit      => 0);
      end if;
   end Get_Operand;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
     (Instruction : Octet)
      return Data_Size
   is
   begin
      return Data_Size'Val (Instruction / 64 - 1);
   end Get_Size;

   ----------
   -- Read --
   ----------

   procedure Read
     (Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class;
      Value   :    out Word)
   is
   begin
      if Operand.Mode = Literal then
         Value := Word (Operand.Lit);
      elsif Operand.Mode = Register
        and then not Operand.Deferred
      then
         Value := Get (R (Operand.Register), Size);
      else
         declare
            A : constant Address :=
                  Get_Address (Operand, Size, Trace, R, Memory);
         begin
            Value := Memory.Get_Value (A, Size);
         end;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      R       : in out Registers;
      Memory  : in out Memory_Interface'Class;
      Value   : Word)
   is
   begin
      if Operand.Mode = Literal then
         raise Constraint_Error with "cannot update a literal operand";
      elsif Operand.Mode = Register
        and then not Operand.Deferred
      then
         Set (R (Operand.Register), Size, Value);
      else
         declare
            A : constant Address :=
                  Get_Address (Operand, Size, Trace, R, Memory);
         begin
            Memory.Set_Value (A, Size, Value);
         end;
      end if;
   end Write;

end Aqua.Architecture;
