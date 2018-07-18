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
                          To_Instruction (Op);
         A_Img        : constant String :=
                          Ada.Characters.Handling.To_Lower
                            (Aqua_Instruction'Image (Instruction));
         Img          : constant String := A_Img (3 .. A_Img'Last);

      begin
         return Img;
      end;
   exception
      when others =>
         return "-";
   end Opcode_Image;

end Aqua.Debug;
