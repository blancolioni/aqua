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
               return A_Halt;
            when 3 =>
               case Low_Nybble is
                  when 0 =>
                     return A_Halt;
                  when 1 =>
                     return A_Halt;
                  when 2 =>
                     return A_Halt;
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
         return A_Halt;
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
               if Low_Nybble < 8 then
                  declare
                     Result : constant Triple_Operand_Instruction :=
                                Triple_Operand_Instruction'Val
                                  (Aqua_Instruction'Pos
                                     (Triple_Operand_Instruction'First)
                                   + Low_Nybble);
                  begin
                     return Result;
                  end;
               else
                  declare
                     Result : constant Triple_Set_Instruction :=
                                Triple_Set_Instruction'Val
                                  (Aqua_Instruction'Pos
                                     (Triple_Set_Instruction'First)
                                   + (Low_Nybble - 8));
                  begin
                     return Result;
                  end;
               end if;
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
         when Triple_Set_Instruction =>
            return (Data_Size'Pos (Size) + 1) * 64
              + 2#0011_1000#
            + (Aqua_Instruction'Pos (Instruction)
               - Aqua_Instruction'Pos (Triple_Set_Instruction'First));
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
      if Operand.Mode = Small_Immediate then
         return Operand.Lit;
      elsif not Operand.Deferred then
         return Addressing_Mode'Pos (Operand.Mode) * 32
           + Octet (Operand.Register);
      else
         pragma Assert (Operand.Mode in Deferrable_Mode);
         case Deferrable_Mode (Operand.Mode) is
            when Register =>
               return 6 * 32 + Octet (Operand.Register);
            when Indexed_8 =>
               return 7 * 32 + Octet (Operand.Register);
         end case;
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
      Memory  : in out Aqua.Memory.Memory_Type'Class)
      return Address
   is
      Result : Address;
      Auto_Size : constant Word :=
                    (if Operand.Deferred
                     then Data_Octets (Address_Size)
                     else Data_Octets (Size));
   begin
      case Operand.Mode is
         when Small_Immediate =>
            raise Constraint_Error with
              "cannot get address of literal mode";
         when Register =>
            if Operand.Deferred then
               if Trace then
                  Ada.Text_IO.Put
                    (" " & Aqua.IO.Hex_Image (R (Operand.Register)));
               end if;
               return R (Operand.Register);
            else
               raise Program_Error
                 with "cannot get address of register mode";
            end if;
         when Postincrement =>
            Result := R (Operand.Register);
            R (Operand.Register) := R (Operand.Register) + Auto_Size;
         when Predecrement =>
            R (Operand.Register) := R (Operand.Register) - Auto_Size;
            Result := R (Operand.Register);
         when Indexed_8 | Indexed_32 =>
            Result := R (Operand.Register);
            declare
               Index_Size : constant Data_Size :=
                              (if Operand.Mode = Indexed_32
                               then Word_32_Size
                               else Word_8_Size);
               A : constant Word :=
                              Memory.Get_Value (R (R_PC), Index_Size);
            begin
               if Operand.Mode = Indexed_8 then
                  if A < 128 then
                     Result := Result + Address (A);
                  else
                     Result := Result - Address (256 - A);
                  end if;
               else
                  Result := Result + A;
               end if;

               R (R_PC) := R (R_PC) + Data_Octets (Index_Size);
            end;
      end case;

      if Operand.Deferred then
         Result := Memory.Get_Value (Result, Word_32_Size);
      end if;

      if Trace then
         Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Result));
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
                 Mode     => Small_Immediate,
                 Lit      => Op);
      else
         declare
            Mode : constant Octet := Op / 32;
         begin
            return (Register => Register_Index (Op mod 32),
                    Deferred => Mode in 6 .. 7,
                    Mode     =>
                      (if Mode = 7 then Indexed_8
                       elsif Mode = 6 then Register
                       else Addressing_Mode'Val (Mode)),
                    Lit      => 0);
         end;
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
      Memory  : in out Aqua.Memory.Memory_Type'Class;
      Value   :    out Word)
   is
   begin
      if Operand.Mode = Small_Immediate then
         Value := Word (Operand.Lit);
      else
         if Operand.Mode = Register
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
      end if;
   end Read;

   -------------------
   -- Register_Name --
   -------------------

   function Register_Name (R : Register_Index) return String is
      Name : String := Register_Index'Image (R);
   begin
      Name (Name'First) := 'r';
      case R is
         when 0 .. 28 =>
            return Name;
         when R_PC =>
            return "pc";
         when R_SP =>
            return "sp";
         when R_FP =>
            return "fp";
      end case;
   end Register_Name;

   -----------
   -- Write --
   -----------

   procedure Write
     (Operand : Operand_Type;
      Size    : Data_Size;
      Trace   : Boolean;
      R       : in out Registers;
      Memory  : in out Aqua.Memory.Memory_Type'Class;
      Value   : Word)
   is
   begin
      if Operand.Mode = Small_Immediate then
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
