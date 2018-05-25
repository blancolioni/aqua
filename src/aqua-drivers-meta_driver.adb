with Aqua.IO;

package body Aqua.Drivers.Meta_Driver is

   Meta_Driver_Address : constant := 16#F000_0000#;

   type Root_Meta_Driver is
     new Root_Aqua_Driver with
      record
         Memory : access Aqua.Memory.Memory_Type'Class;
         Next   : Address;
      end record;

   overriding function Identity
     (Driver : Root_Meta_Driver)
      return String
   is ("aqua-meta-driver");

   overriding function Monitored
     (Driver   : Root_Meta_Driver;
      Register : Driver_Register_Range)
      return Boolean
   is (Register < 4);

   overriding procedure Update
     (Driver : in out Root_Meta_Driver);

   type Meta_Driver_Access is access all Root_Meta_Driver'Class;

   ------------------------
   -- Create_Meta_Driver --
   ------------------------

   procedure Create_Meta_Driver
     (Address_Space : not null access Aqua.Memory.Memory_Type'Class)
   is
      Driver : constant Meta_Driver_Access :=
                 new Root_Meta_Driver (12);
   begin
      Driver.Memory := Address_Space;
      Driver.Next   := Meta_Driver_Address + 16#0100#;

      Address_Space.Install_Driver
        (Start  => Meta_Driver_Address,
         Driver => Aqua_Driver (Driver));
   end Create_Meta_Driver;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Driver : in out Root_Meta_Driver)
   is
      A      : Address := Driver.Get_Word (8);
      Length : constant Word := Driver.Get_Word (4) mod 256;
      Name   : String (1 .. Natural (Length));
   begin
      for Ch of Name loop
         Ch := Character'Val (Driver.Memory.Get_Word (A) mod 256);
         A := A + 4;
      end loop;

      declare
         Install_Address : constant Address := Driver.Next;
         New_Driver      : constant Aqua_Driver :=
                             Create (Name);
         Size            : Word := 1;
      begin
         if New_Driver = null then
            Driver.Log ("no such driver: " & Name);
            Driver.Set_Word (0, 0);
         else
            while Size <= Word (New_Driver.Last_Address) loop
               Size := Size * 2;
            end loop;
            Driver.Next := Driver.Next + Size;
            Driver.Memory.Install_Driver
              (Start  => Install_Address,
               Driver => New_Driver);
            Driver.Set_Word (0, Install_Address);
            Driver.Log ("installed " & Name & " at "
                        & Aqua.IO.Hex_Image (Install_Address));
         end if;
      end;
   end Update;

end Aqua.Drivers.Meta_Driver;
