with Ada.Wide_Wide_Text_IO;

package body Aqua.Drivers is

   type Text_Writer_Registers is
     array (Address range 0 .. 7) of Octet;

   type Text_Writer_Driver is
     new Aqua_Driver_Interface with
      record
         Rs : Text_Writer_Registers := (others => 255);
      end record;

   overriding function Address_Count
     (Driver : Text_Writer_Driver)
      return Address
   is (Text_Writer_Registers'Length);

   overriding function Get_Octet
     (Driver : Text_Writer_Driver;
      Addr   : Address)
      return Octet
   is (Driver.Rs (Addr));

   overriding procedure Set_Octet
     (Driver : in out Text_Writer_Driver;
      Addr   : Address;
      Value  : Octet);

   ---------------
   -- Set_Octet --
   ---------------

   overriding procedure Set_Octet
     (Driver : in out Text_Writer_Driver;
      Addr   : Address;
      Value  : Octet)
   is
   begin
      Driver.Rs (Addr) := Value;
      if Addr = 0 and then Value /= 0 then
         declare
            W : Word := 0;
         begin
            for X in reverse Address range 4 .. 7 loop
               W := W * 256 + Word (Driver.Rs (X));
            end loop;
            Ada.Wide_Wide_Text_IO.Put (Wide_Wide_Character'Val (W));
         end;
      end if;
   end Set_Octet;

   -----------------
   -- Text_Writer --
   -----------------

   function Text_Writer return Aqua_Driver is
   begin
      return new Text_Writer_Driver;
   end Text_Writer;

end Aqua.Drivers;
