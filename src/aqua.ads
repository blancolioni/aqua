package Aqua is

   Word_Size      : constant := 32;
   Octets_Per_Word : constant := Word_Size / 8;

   type Word is mod 2 ** Word_Size;
   type Word_16 is mod 65536;
   type Octet is mod 2 ** 8;

   subtype Address is Word;

   type Data_Size is
     (Word_8_Size, Word_16_Size, Word_32_Size);

   Float_32_Size : constant Data_Size := Word_32_Size;
   Float_64_Size : constant Data_Size := Word_16_Size;

   Address_Size : constant Data_Size := Word_32_Size;

   Data_Octets : constant array (Data_Size) of Word :=
                   (1, 2, 4);

   type Bit_Index is range 0 .. 31;

   type Array_Of_Words is array (Positive range <>) of Word;

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

end Aqua;
