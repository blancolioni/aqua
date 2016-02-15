package body Aqua.Memory is

   --------------
   -- Get_Octet --
   --------------

   function Get_Octet
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Octet
   is
   begin
      return Memory.Mem (Addr);
   end Get_Octet;

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

   --------------
   -- Set_Octet --
   --------------

   procedure Set_Octet
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Octet)
   is
   begin
      Memory.Mem (Addr) := Value;
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
