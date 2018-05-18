with Ada.Text_IO;

with Aqua.IO;

package body Aqua.Memory is

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (Memory : in out Memory_Type'Class) is
   begin
      null;
   end Begin_Transaction;

   ---------------------
   -- End_Transaction --
   ---------------------

   procedure End_Transaction (Memory : in out Memory_Type'Class) is
   begin
      for Driver of Memory.Changes loop
         Driver.Update;
      end loop;
      Memory.Changes.Clear;
   end End_Transaction;

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
         Mem.Page_Count := Mem.Page_Count + 1;
         Mem.Table (T) :=
           new Directory_Entry'
             (Pages => (others => null),
              Flags => (others => (others => False)));
      end if;

      if Mem.Table (T).Pages (D) = null then
         Mem.Table (T).Pages (D) :=
           new Page_Type'(Data => (others => 0),
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
      T : constant Address := Table_Address (Addr);
      D : constant Address := Directory_Address (Addr);
   begin
      if Memory.Table (T) = null then
         return False;
      else
         return Memory.Table (T).Flags (D) (Flag);
      end if;
   end Flag_Is_Set;

   --------------
   -- Get_Octet --
   --------------

   function Get_Octet
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Octet
   is
      Page      : constant Page_Access := Get_Page (Memory, Addr);
   begin
      if Page = null then
         return 0;
      elsif not Memory.Flag_Is_Set (Addr, Flag_R) then
         raise Constraint_Error with
           "page not readable: address: " & Aqua.IO.Hex_Image (Addr);
      else
         if Memory.Flag_Is_Set (Addr, Flag_Driver) then
            for Position in Page.Driver_Map.Iterate loop
               declare
                  Driver : constant Aqua.Drivers.Aqua_Driver :=
                             Driver_Maps.Element (Position);
                  First   : constant Address := Driver_Maps.Key (Position);
                  Last    : constant Address :=
                              First + Address (Driver.Last_Address);
               begin
                  if Addr in First .. Last then
                     return Driver.Get_Octet
                       (Aqua.Drivers.Driver_Register_Range (Addr - First));
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
         return Mem.Table (T).Pages (D);
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
                     (Start + Address (Driver.Last_Address)) / Page_Size;
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

         Memory.Table (Table_Address (Start))
           .Flags (Directory_Address (Start)) :=
             (Flag_R => True, Flag_W => True, Flag_X => False,
              Flag_Driver => True);

         Page.Driver_Map.Insert (Start, Driver);

      end;

   end Install_Driver;

   ----------------------
   -- Set_Access_Flags --
   ----------------------

   procedure Set_Access_Flags
     (Memory  : in out Memory_Type'Class;
      Base    : Address;
      Bound   : Address;
      R, W, X : Boolean)
   is
      Page : Address := (Base / Page_Size) * Page_Size;
   begin
      while Page < Bound loop
         Memory.Set_Flag (Page, Flag_R, R);
         Memory.Set_Flag (Page, Flag_W, W);
         Memory.Set_Flag (Page, Flag_X, X);
         Page := Page + Page_Size;
      end loop;
   end Set_Access_Flags;

   --------------
   -- Set_Flag --
   --------------

   procedure Set_Flag
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Flag   : Page_Flag;
      Value  : Boolean)
   is
      T : constant Table_Address_Range := Table_Address (Addr);
      D : constant Directory_Address_Range := Directory_Address (Addr);
   begin
      if Memory.Table (T) = null then
         Memory.Page_Count := Memory.Page_Count + 1;
         Memory.Table (T) :=
           new Directory_Entry'
             (Pages => (others => null),
              Flags => (others => (others => False)));
      end if;
      Memory.Table (T).Flags (D) (Flag) := Value;
   end Set_Flag;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
     (Memory  : in out Memory_Type'Class;
      Addr    : Address;
      R, W, X : Boolean := False)
   is
   begin
      if R then
         Memory.Set_Flag (Addr, Flag_R, True);
      end if;
      if W then
         Memory.Set_Flag (Addr, Flag_W, True);
      end if;
      if X then
         Memory.Set_Flag (Addr, Flag_X, True);
      end if;
   end Set_Flags;

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
      if not Memory.Flag_Is_Set (Addr, Flag_W) then
         raise Constraint_Error with
           "page not writable: address: " & Aqua.IO.Hex_Image (Addr);
      end if;

      declare
         Page : constant Page_Access := Memory.Get_Page (Addr);
      begin

         if False then
            Ada.Text_IO.Put_Line ("[" & Aqua.IO.Hex_Image (Addr)
                                  & "]<-"
                                  & Aqua.IO.Hex_Image
                                    (Word (Value), Word_8_Size));
         end if;

         Page.Data (Addr mod Page_Size) := Value;
         if Memory.Flag_Is_Set (Addr, Flag_Driver) then
            for Position in Page.Driver_Map.Iterate loop
               declare
                  Driver : constant Aqua.Drivers.Aqua_Driver :=
                             Driver_Maps.Element (Position);
                  First   : constant Address := Driver_Maps.Key (Position);
                  Last    : constant Address :=
                              First + Address (Driver.Last_Address);
               begin
                  if Addr in First .. Last then
                     Driver.Set_Octet
                       (Aqua.Drivers.Driver_Register_Range (Addr - First),
                        Value);
                     if Driver.Monitored
                       (Aqua.Drivers.Driver_Register_Range (Addr - First))
                       and then not Memory.Changes.Contains (Driver)
                     then
                        Memory.Changes.Append (Driver);
                     end if;
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
