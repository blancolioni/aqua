with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;

with WL.String_Maps;

package body Aqua.Drivers is

   package Driver_Registry_Maps is
     new WL.String_Maps (Driver_Creator);

   Driver_Registry : Driver_Registry_Maps.Map;

   Text_Writer_Register_Count : constant Driver_Register_Count := 255;

   R_Character : constant Driver_Register_Range := 0;
   R_Col       : constant Driver_Register_Range := 4;
   R_Output    : constant Driver_Register_Range := 8;
   R_Name      : constant Driver_Register_Range := 12;

   type File_Access is access Ada.Text_IO.File_Type;

   type Text_Writer_Driver is
     new Root_Aqua_Driver with
      record
         Output     : File_Access;
         Redirected : Boolean := False;
      end record;

   overriding function Identity
     (Driver : Text_Writer_Driver)
      return String
   is ("aqua-text-writer");

   overriding procedure Update
     (Driver : in out Text_Writer_Driver);

   Character_Handling_Register_Count : constant Driver_Register_Count := 8;

   R_Command        : constant Driver_Register_Range := 0;
   R_Character_Code : constant Driver_Register_Range := 4;

   type Character_Handling_Command is
     (No_Command, To_Upper_Case, To_Lower_Case, Is_White_Space);

   type Character_Handling_Driver is
     new Root_Aqua_Driver with null record;

   overriding function Identity
     (Driver : Character_Handling_Driver)
      return String
   is ("aqua-character-handler");

   overriding procedure Update
     (Driver : in out Character_Handling_Driver);

   ------------------------
   -- Character_Handling --
   ------------------------

   function Character_Handling return Aqua_Driver is
   begin
      return new Character_Handling_Driver (Character_Handling_Register_Count);
   end Character_Handling;

   -------------------
   -- Clear_Changes --
   -------------------

   procedure Clear_Changes
     (Driver : in out Root_Aqua_Driver'Class)
   is
   begin
      Driver.Changed := (others => False);
   end Clear_Changes;

   ------------
   -- Create --
   ------------

   function Create
     (Identifier : String)
      return Aqua_Driver
   is
   begin
      if Driver_Registry.Contains (Identifier) then
         return Driver_Registry.Element (Identifier).all;
      else
         return null;
      end if;
   end Create;

   --------------
   -- Get_Word --
   --------------

   function Get_Word
     (Driver : Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range)
      return Word
   is
   begin
      return X : Word := 0 do
         for I in reverse Driver_Register_Range range 0 .. 3 loop
            X := X * 256 + Word (Driver.Get_Octet (Addr + I));
         end loop;
      end return;
   end Get_Word;

   ---------
   -- Log --
   ---------

   procedure Log
     (Driver  : Root_Aqua_Driver'Class;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Driver.Identity & ": " & Message);
   end Log;

   -----------------
   -- Read_String --
   -----------------

   function Read_String
     (Driver : Root_Aqua_Driver'Class;
      Start  : Driver_Register_Range)
      return String
   is
      use Aqua.Drivers;
      A : Driver_Register_Range := Start;
      L : constant Natural := Natural (Driver.Get_Word (A)) mod 256;
      S : String (1 .. L);
   begin
      for Ch of S loop
         A := A + 4;
         Ch := Character'Val (Driver.Get_Word (A));
      end loop;
      return S;
   end Read_String;

   --------------
   -- Register --
   --------------

   procedure Register
     (Identifier : String;
      Creator    : Driver_Creator)
   is
   begin
      Driver_Registry.Insert (Identifier, Creator);
   end Register;

   ---------------
   -- Set_Octet --
   ---------------

   procedure Set_Octet
     (Driver : in out Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range;
      Value  : Octet)
   is
   begin
      Driver.Rs (Addr) := Value;
      Driver.Changed (Addr) := True;
   end Set_Octet;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Driver : in out Root_Aqua_Driver'Class;
      Addr   : Driver_Register_Range;
      Value  : Word)
   is
      It : Word := Value;
   begin
      for I in Driver_Register_Range range 0 .. 3 loop
         Driver.Set_Octet (Addr + I, Octet (It mod 256));
         It := It / 256;
      end loop;
   end Set_Word;

   -----------------
   -- Text_Writer --
   -----------------

   function Text_Writer return Aqua_Driver is
   begin
      return new Text_Writer_Driver (Text_Writer_Register_Count - 1);
   end Text_Writer;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Driver : in out Character_Handling_Driver)
   is
   begin
      if Driver.Changed_Word (R_Command) then
         declare
            use Ada.Wide_Wide_Characters.Handling;
            Last_Command  : constant Word :=
                              Character_Handling_Command'Pos
                                (Character_Handling_Command'Last);
            Command_Value : constant Word := Driver.Get_Word (R_Command);
            Ch            : Wide_Wide_Character :=
                              Wide_Wide_Character'Val
                                (Driver.Get_Word (R_Character_Code));
         begin
            if Command_Value in 1 .. Last_Command then
               case Character_Handling_Command'Val (Command_Value) is
                  when No_Command =>
                     null;
                  when To_Upper_Case =>
                     Ch := To_Upper (Ch);
                  when To_Lower_Case =>
                     Ch := To_Lower (Ch);
                  when Is_White_Space =>
                     Ch := (if Is_Space (Ch) then ' ' else 'x');
               end case;

               Driver.Set_Word
                 (R_Character_Code,
                  Wide_Wide_Character'Pos (Ch));
               Driver.Set_Word (R_Command, 0);
            end if;
            Driver.Clear_Changes;
         end;
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Driver : in out Text_Writer_Driver)
   is
   begin
      if Driver.Changed_Word (R_Col) then
         declare
            New_Col : constant Word := Driver.Get_Word (R_Col);
         begin
            if New_Col > 0 then
               Ada.Wide_Wide_Text_IO.Set_Col
                 (Ada.Wide_Wide_Text_IO.Count (New_Col));
            end if;
         end;
      end if;

      if Driver.Changed_Word (R_Character) then
         declare
            W : constant Word := Driver.Get_Word (R_Character);
         begin
            if Driver.Redirected then
               if W = 10 then
                  Ada.Text_IO.New_Line (Driver.Output.all);
               else
                  Ada.Text_IO.Put
                    (Driver.Output.all,
                     Character'Val (Driver.Get_Word (R_Character) mod 256));
               end if;
            else
               if W = 10 then
                  Ada.Wide_Wide_Text_IO.New_Line;
               else
                  Ada.Wide_Wide_Text_IO.Put
                    (Wide_Wide_Character'Val (Driver.Get_Word (R_Character)));
               end if;
            end if;
         end;
      end if;

      if Driver.Changed_Word (R_Output) then
         if Driver.Redirected then
            Ada.Text_IO.Close (Driver.Output.all);
            Driver.Redirected := False;
         end if;

         if Driver.Get_Word (R_Name) /= 0 then
            declare
               Name : constant String := Driver.Read_String (R_Name);
            begin
               if Driver.Output = null then
                  Driver.Output := new Ada.Text_IO.File_Type;
               end if;

               Ada.Text_IO.Create
                 (Driver.Output.all, Ada.Text_IO.Out_File, Name);
               Driver.Redirected := True;
            end;
         end if;
      end if;

      Driver.Set_Word
        (R_Col, Word (Ada.Wide_Wide_Text_IO.Col));
      Driver.Clear_Changes;
   end Update;

end Aqua.Drivers;
