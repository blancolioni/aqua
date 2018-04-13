with Aqua.IO;

package body Aqua.Memory is

   -----------------
   -- Ensure_Page --
   -----------------

   procedure Ensure_Page
     (Mem  : in out Memory_Type;
      Addr : Address)
   is
      T : constant Table_Address_Range := Table_Address (Addr);
      D : constant Directory_Address_Range := Directory_Address (Addr);
   begin
      if Mem.Table (T) = null then
         Mem.Table (T) := new Directory_Type'(others => null);
      end if;

      if Mem.Table (T) (D) = null then
         Mem.Table (T) (D) :=
           new Page_Type'(Data => (others => 0),
                          Flags => (others => <>),
                          Driver_Map => <>);
         Mem.Page_Count := Mem.Page_Count + 1;
      end if;
   end Ensure_Page;

   -----------------
   -- Flag_Is_Set --
   -----------------

   function Flag_Is_Set
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag)
      return Boolean
   is
      Page : constant Page_Access := Memory.Get_Page (Addr);
   begin
      return Page /= null and then Page.Flags (Flag);
   end Flag_Is_Set;

   --------------
   -- Get_Octet --
   --------------

   function Get_Octet
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Octet
   is
      Page : constant Page_Access := Get_Page (Memory, Addr);
   begin
      if Page = null then
         return 0;
      else
         if Page.Flags (Flag_Driver) then
            for Position in Page.Driver_Map.Iterate loop
               declare
                  Driver : constant Aqua.Drivers.Aqua_Driver :=
                             Driver_Maps.Element (Position);
                  Base   : constant Address := Driver_Maps.Key (Position);
                  Bound  : constant Address :=
                             Base + Driver.Address_Count;
               begin
                  if Addr in Base .. Bound then
                     return Driver.Get_Octet (Addr - Base);
                  end if;
               end;
            end loop;
         end if;
         return Page.Data (Addr mod Page_Size);
      end if;
   end Get_Octet;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Mem  : Memory_Type;
      Addr : Address)
      return Page_Access
   is
      T : constant Table_Address_Range := Table_Address (Addr);
      D : constant Directory_Address_Range := Directory_Address (Addr);
   begin
      if Mem.Table (T) = null then
         return null;
      else
         return Mem.Table (T) (D);
      end if;
   end Get_Page;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size)
      return Word
   is
      It : Word := 0;
   begin
      case Size is
         when Word_8_Size =>
            It := Word (Memory.Get_Octet (Addr));
         when Word_16_Size =>
            It := Word (Memory.Get_Octet (Addr))
              + 256 * Word (Memory.Get_Octet (Addr + 1));
         when Word_32_Size =>
            It := Word (Memory.Get_Octet (Addr))
              + 256 * Word (Memory.Get_Octet (Addr + 1))
              + 65536 * Word (Memory.Get_Octet (Addr + 2))
              + 256 * 65536 * Word (Memory.Get_Octet (Addr + 3));
      end case;

      return It;
   end Get_Value;

   --------------------
   -- Install_Driver --
   --------------------

   procedure Install_Driver
     (Memory : in out Memory_Type'Class;
      Start  : Address;
      Driver : Aqua.Drivers.Aqua_Driver)
   is
      Start_Page : constant Address := Start / Page_Size;
      End_Page   : constant Address :=
                     (Start + Driver.Address_Count - 1) / Page_Size;
      Start_Address : constant Address :=
                        Start mod Page_Size;
   begin
      if Start_Page /= End_Page then
         raise Constraint_Error with
           "driver crosses page boundary";
      end if;

      Memory.Ensure_Page (Start);

      declare
         Page : constant Page_Access := Memory.Get_Page (Start);
      begin
         if Page.Driver_Map.Contains (Start_Address) then
            raise Constraint_Error with
              "driver conflict at " & Aqua.IO.Hex_Image (Start);
         end if;

         Page.Flags (Flag_Driver) := True;
         Page.Flags (Flag_R) := True;
         Page.Flags (Flag_W) := True;
         Page.Flags (Flag_X) := False;

         Page.Driver_Map.Insert (Start, Driver);

      end;

   end Install_Driver;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag;
      Value  : Boolean)
   is
   begin
      Ensure_Page (Memory, Addr);
      Memory.Get_Page (Addr).Flags (Flag) := Value;
   end Set_Flag;

   ---------------
   -- Set_Octet --
   ---------------

   procedure Set_Octet
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Octet)
   is
   begin
      Ensure_Page (Memory, Addr);
      declare
         Page : constant Page_Access := Memory.Get_Page (Addr);
      begin
         Page.Data (Addr mod Page_Size) := Value;
         if Page.Flags (Flag_Driver) then
            for Position in Page.Driver_Map.Iterate loop
               declare
                  Driver : constant Aqua.Drivers.Aqua_Driver :=
                             Driver_Maps.Element (Position);
                  Base : constant Address := Driver_Maps.Key (Position);
                  Bound : constant Address :=
                             Base + Driver.Address_Count;
               begin
                  if Addr in Base .. Bound then
                     Driver.Set_Octet (Addr - Base, Value);
                  end if;
               end;
            end loop;
         end if;
      end;

   end Set_Octet;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size;
      Value  : Word)
   is
      It : Word := Value;
   begin
      for I in Address range 0 .. Address (Data_Octets (Size)) - 1 loop
         Memory.Set_Octet (Addr + I, Octet (It mod 256));
         It := It / 256;
      end loop;
   end Set_Value;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Word)
   is
   begin
      Set_Value (Memory, Addr, Word_32_Size, Value);
   end Set_Word;

end Aqua.Memory;
