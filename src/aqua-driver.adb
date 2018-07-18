with Ada.Command_Line;
with Ada.Text_IO;

with Aqua.Architecture;
with Aqua.CPU;
with Aqua.Debug;
with Aqua.Images;
with Aqua.IO;

with Aqua.Drivers;
with Aqua.Paths;

procedure Aqua.Driver is

begin
   Aqua.IO.Set_IO_Path (Aqua.Paths.Config_Path);

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "opcodes"
   then
      for Op in Octet loop
         Ada.Text_IO.Put_Line
           (Aqua.IO.Hex_Image (Op) & ": " & Aqua.Debug.Opcode_Image (Op));
      end loop;
      return;
   end if;

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "to_instruction"
   then
      declare
         use Ada.Text_IO;
         Ops : array (Octet) of Aqua.Architecture.Aqua_Instruction :=
                 (others => Aqua.Architecture.A_TRAP);
      begin
         for Instr in Aqua.Architecture.Aqua_Instruction loop
            Ops (Aqua.Architecture.To_Opcode (Instr)) := Instr;
         end loop;

         Put_Line ("separate (Aqua.Architecture)");
         Put_Line ("function To_Instruction");
         Put_Line ("  (Opcode : Octet)");
         Put_Line ("  return Aqua_Instruction");
         Put_Line ("is");
         Put_Line ("   Map : constant array (Octet) of Aqua_Instruction :=");
         for Op in Octet loop
            if Op = 0 then
               Put ("     (");
            else
               Put ("      ");
            end if;
            declare
               Op_Image : constant String := Octet'Image (Op);
            begin
               Put (Op_Image (2 .. Op_Image'Last) & " => "
                    & Aqua.Architecture.Aqua_Instruction'Image
                      (Ops (Op)));
            end;
            if Op = 255 then
               Put_Line (");");
            else
               Put_Line (",");
            end if;
         end loop;
         Put_Line ("begin");
         Put_Line ("   return Map (Opcode);");
         Put_Line ("end To_Instruction;");
      end;

      return;
   end if;

   declare
      Image  : constant Aqua.Images.Image_Type := Aqua.Images.New_Image;
      CPU    : Aqua.CPU.Aqua_CPU_Type (Image);
   begin
      Image.Load ("test.o32");
      Image.Link;
      CPU.Execute ("self-test", 16#1000#, (1 => 0));
   end;

end Aqua.Driver;
