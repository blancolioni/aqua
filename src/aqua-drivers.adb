with Ada.Wide_Wide_Text_IO;

package body Aqua.Drivers is

   Text_Writer_Register_Count : constant Driver_Register_Count := 8;

   R_Character : constant Driver_Register_Range := 0;
   R_Col       : constant Driver_Register_Range := 4;

   type Text_Writer_Driver is
     new Root_Aqua_Driver with null record;

   overriding procedure Update
     (Driver : in out Text_Writer_Driver);

   -------------------
   -- Clear_Changes --
   -------------------

   procedure Clear_Changes
     (Driver : in out Root_Aqua_Driver'Class)
   is
   begin
      Driver.Changed := (others => False);
   end Clear_Changes;

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
            if W = 10 then
               Ada.Wide_Wide_Text_IO.New_Line;
            else
               Ada.Wide_Wide_Text_IO.Put
                 (Wide_Wide_Character'Val (Driver.Get_Word (R_Character)));
            end if;
         end;
      end if;

      Driver.Set_Word
        (R_Col, Word (Ada.Wide_Wide_Text_IO.Col));
      Driver.Clear_Changes;
   end Update;

end Aqua.Drivers;
