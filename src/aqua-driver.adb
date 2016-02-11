with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Text_IO;

with Aqua.Architecture;
with Aqua.CPU;
with Aqua.Debug;
with Aqua.Images;
with Aqua.IO;
with Aqua.Loaders;
with Aqua.Primitives.Init;

with Aqua.Paths;

procedure Aqua.Driver is

   function Mixed_Case_Image
     (Instruction : Aqua.Architecture.Aqua_Instruction)
      return String;

   ----------------------
   -- Mixed_Case_Image --
   ----------------------

   function Mixed_Case_Image
     (Instruction : Aqua.Architecture.Aqua_Instruction)
      return String
   is
      use Ada.Characters.Handling;
      Result : String :=
                 Aqua.Architecture.Aqua_Instruction'Image (Instruction);
      Start  : Boolean := True;
   begin
      for I in Result'Range loop
         if Start then
            Result (I) := To_Upper (Result (I));
            Start := False;
         elsif Result (I) = '_' then
            Start := True;
         else
            Result (I) := To_Lower (Result (I));
         end if;
      end loop;
      return Result;
   end Mixed_Case_Image;

begin
   Aqua.IO.Set_IO_Path (Aqua.Paths.Config_Path);
   Aqua.Primitives.Init.Create_Primitives;

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
     and then Ada.Command_Line.Argument (1) = "get_instruction"
   then
      declare
         use Ada.Text_IO;
      begin
         Put_Line ("separate (Aqua.Architecture)");
         Put_Line ("function Get_Instruction");
         Put_Line ("  (Instruction : Octet)");
         Put_Line ("  return Aqua_Instruction");
         Put_Line ("is");
         Put_Line ("begin");
         Put_Line ("   case Instruction is");
         for Op in Octet loop
            Put_Line ("      when" & Op'Img & " =>");
            declare
               Instruction : Aqua.Architecture.Aqua_Instruction;
            begin
               Instruction :=
                 Aqua.Architecture.Get_Instruction
                   (Op);
               Put_Line ("         return "
                         & Mixed_Case_Image
                           (Instruction)
                         & ";");
            exception
               when others =>
                  Put_Line ("         raise Bad_Instruction with """
                            & Aqua.IO.Hex_Image (Op)
                            & """;");
            end;
         end loop;
         Put_Line ("   end case;");
         Put_Line ("end Get_Instruction;");
      end;

      return;
   end if;

   declare
      Loader : constant Aqua.Loaders.Null_Loader_Access :=
                 new Aqua.Loaders.Null_Loader;
      Image  : constant Aqua.Images.Image_Type := Aqua.Images.New_Image;
      CPU    : Aqua.CPU.Aqua_CPU_Type (Image, Loader);
   begin
      Image.Load ("test.o32");
      Image.Link;
      CPU.Execute (16#1000#, (1 => 0));
   end;

end Aqua.Driver;
