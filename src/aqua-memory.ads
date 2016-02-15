package Aqua.Memory is

   type Memory_Type is tagged private;

   function Get_Octet
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Octet
     with Inline_Always;

   procedure Set_Octet
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Octet)
     with Inline_Always;

   function Get_Value
     (Memory : Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size)
      return Word
     with Inline_Always;

   procedure Set_Value
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Size   : Data_Size;
      Value  : Word)
     with Inline_Always;

   function Get_Word
     (Memory : Memory_Type'Class;
      Addr   : Address)
      return Word
   is (Get_Value (Memory, Addr, Word_32_Size))
   with Inline_Always;

   procedure Set_Word
     (Memory : in out Memory_Type'Class;
      Addr   : Address;
      Value  : Word)
     with Inline_Always;

private

   type Memory_Array is array (Address) of Octet;

   type Memory_Type is tagged
      record
         Mem : Memory_Array;
      end record;

end Aqua.Memory;
