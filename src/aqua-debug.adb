with Ada.Characters.Handling;

with Aqua.Architecture;

package body Aqua.Debug is

   ------------------
   -- Opcode_Image --
   ------------------

   function Opcode_Image (Op : Octet) return String is
   begin
      declare
         use Aqua.Architecture;
         Instruction  : constant Aqua_Instruction :=
                          Calculate_Instruction (Op);
         A_Img        : constant String :=
                          Ada.Characters.Handling.To_Lower
                            (Aqua_Instruction'Image (Instruction));
         Img          : constant String := A_Img (3 .. A_Img'Last);

         function Size_Image (Size : Data_Size) return String
         is (case Size is
                when Word_8_Size  => ".1",
                when Word_16_Size => ".2",
                when Word_32_Size => "");

      begin
         case Instruction is
            when No_Operand_Instruction =>
               return Img;
            when Single_Operand_Instruction =>
               declare
                  Size : constant Data_Size := Get_Size (Op);
               begin
                  return Img & Size_Image (Size) & " dst";
               end;

            when Double_Operand_Instruction =>
               declare
                  Size : constant Data_Size := Get_Size (Op);
               begin
                  return Img & Size_Image (Size) & " src, dst";
               end;
            when Triple_Operand_Instruction =>
               declare
                  Size  : constant Data_Size := Get_Size (Op);
                  T_Img : constant String :=
                            Img (Img'First .. Img'Last - 2);
               begin
                  return T_Img & Size_Image (Size) & " src1, src2, dst";
               end;
            when Triple_Set_Instruction =>
               declare
                  Size  : constant Data_Size := Get_Size (Op);
                  T_Img : constant String :=
                            Img (Img'First .. Img'Last - 2);
               begin
                  return T_Img & Size_Image (Size) & " src1, src2, dst";
               end;
            when Single_Operand_Float_Instruction =>
               declare
                  F_Img : String := Img;
               begin
                  if Op mod 2 = 1 then
                     F_Img (F_Img'First) := 'd';
                  end if;
                  return F_Img & " dst";
               end;
            when Double_Operand_Float_Instruction =>
               declare
                  F_Img : String := Img;
               begin
                  if Op mod 2 = 1 then
                     F_Img (F_Img'First) := 'd';
                  end if;
                  return F_Img & " src, dst";
               end;

            when Branch_Instruction =>
               return Img & " offset";
            when A_Jmp | A_Jsr | A_Goto =>
               return Img & " destination";
            when A_Call =>
               return Img;
            when A_Trap =>
               return Img & Octet'Image (Op mod 16);
         end case;
      end;
   exception
      when others =>
         return "-";
   end Opcode_Image;

end Aqua.Debug;
