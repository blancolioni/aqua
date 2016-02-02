package body Aqua.Memory is

   --------------
   -- Get_Octet --
   --------------

   overriding function Get_Octet
     (Memory : Memory_Type;
      Addr   : Address)
      return Octet
   is
   begin
      return Memory.Mem (Addr);
   end Get_Octet;

   --------------
   -- Set_Octet --
   --------------

   overriding procedure Set_Octet
     (Memory : in out Memory_Type;
      Addr   : Address;
      Value  : Octet)
   is
   begin
      Memory.Mem (Addr) := Value;
   end Set_Octet;

end Aqua.Memory;
