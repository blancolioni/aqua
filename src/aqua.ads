package Aqua is

   Word_Size      : constant := 32;
   Octets_Per_Word : constant := Word_Size / 8;

   type Word is mod 2 ** Word_Size;
   type Word_16 is mod 65536;
   type Octet is mod 2 ** 8;

   type Data_Size is
     (Word_8_Size, Word_16_Size, Word_32_Size);

   Float_32_Size : constant Data_Size := Word_32_Size;
   Float_64_Size : constant Data_Size := Word_16_Size;

   Address_Size : constant Data_Size := Word_32_Size;

   Data_Octets : constant array (Data_Size) of Word :=
                   (1, 2, 4);

   type Bit_Index is range 0 .. 31;

   Payload_Bits : constant := 28;
   type Payload is mod 2 ** Payload_Bits;
   Payload_Mask : constant := 16#0FFF_FFFF#;

   type Address is new Payload;
   type External_Reference is new Payload;
   type String_Reference is new Payload;
   type Subroutine_Reference is new Payload;

   type Aqua_Integer is range -2 ** 27 .. 2 ** 27 - 1;

   type Array_Of_Words is array (Positive range <>) of Word;

   Integer_Tag    : constant := 0;
   Address_Tag    : constant := 1;
   String_Tag     : constant := 2;
   External_Tag   : constant := 3;
   Subroutine_Tag : constant := 4;

   function Get_Tag (Value : Word) return Word
   is (Value / 16#1000_0000#);

   function Set_Tag (Value : Word;
                     Tag   : Word)
                     return Word
   is (Value + Tag * 16#1000_0000#);

   function Is_Integer (Value : Word) return Boolean
   is (Get_Tag (Value) = Integer_Tag);

   function Get_Integer (Value : Word) return Aqua_Integer
     with Inline_Always;

   function To_Integer_Word (Value : Aqua_Integer) return Word
     with Inline_Always;

   function Is_Address (Value : Word) return Boolean
   is (Get_Tag (Value) = Address_Tag);

   function Get_Address (Value : Word) return Address with Inline_Always;
   function To_Address_Word (Addr : Address) return Word with Inline_Always;

   function Is_External_Reference (Value : Word) return Boolean
   is (Get_Tag (Value) = External_Tag);

   function Get_External_Reference (Value : Word) return External_Reference
     with Pre => Is_External_Reference (Value);
   function To_External_Word (Reference : External_Reference)
                              return Word with Inline_Always;

   function Is_String_Reference (Value : Word) return Boolean
   is (Get_Tag (Value) = String_Tag);

   function Get_String_Reference (Value : Word) return String_Reference
     with Pre => Is_String_Reference (Value);
   function To_String_Word (Reference : String_Reference) return Word;

   function Get_Bits
     (Value : Word;
      Start : Bit_Index;
      Count : Bit_Index)
      return Word;

   procedure Set
     (Target : in out Word;
      Size   : in     Data_Size;
      Value  : in     Word);

   function Get
     (Source : Word;
      Size   : Data_Size)
      return Word with Inline_Always;

--     type Memory_Interface is interface;
--
--     function Get_Octet (Memory : Memory_Interface;
--                         Addr   : Address)
--                         return Octet
--                         is abstract;
--
--     procedure Set_Octet (Memory : in out Memory_Interface;
--                          Addr   : Address;
--                          Value  : Octet)
--     is abstract;
--
--     function Get_Value (Memory : Memory_Interface'Class;
--                         Addr   : Address;
--                         Size   : Data_Size)
--                         return Word;
--
--     procedure Set_Value (Memory : in out Memory_Interface'Class;
--                          Addr   : Address;
--                          Size   : Data_Size;
--                          Value  : Word);
--
--     function Get_Word (Memory : Memory_Interface'Class;
--                        Addr   : Address)
--                        return Word
--     is (Get_Value (Memory, Addr, Word_32_Size))
--     with Inline_Always;
--
--     procedure Set_Word (Memory : in out Memory_Interface'Class;
--                         Addr   : Address;
--                         Value  : Word);

   type External_Object_Interface is interface;

   function Name (Item : External_Object_Interface) return String
                  is abstract;

   function Text (Item : External_Object_Interface) return String
                  is abstract;

   function Show (Item : External_Object_Interface) return String
                  is abstract;

   procedure Set_Reference
     (Item : in out External_Object_Interface;
      Reference : External_Reference)
   is abstract;

   function Get_Reference
     (Item : External_Object_Interface)
      return External_Reference
      is abstract;

   type External_Object_Access is
     access all External_Object_Interface'Class;

end Aqua;
