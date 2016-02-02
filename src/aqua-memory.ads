package Aqua.Memory is

   type Memory_Type is new Memory_Interface with private;

   overriding function Get_Octet (Memory : Memory_Type;
                                 Addr   : Address)
                                 return Octet;

   overriding procedure Set_Octet (Memory : in out Memory_Type;
                                  Addr   : Address;
                                  Value  : Octet);

private

   type Memory_Array is array (Address) of Octet;

   type Memory_Type is new Memory_Interface with
      record
         Mem : Memory_Array;
      end record;

end Aqua.Memory;
