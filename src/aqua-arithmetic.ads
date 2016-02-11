package Aqua.Arithmetic is

   --  type-aware arithmetic

   procedure Inc (X : in out Word;
                  Y : in     Positive := 1) with Inline_Always;

   procedure Dec (X : in out Word;
                  Y : in     Positive := 1);

   function Relative_Address
     (Location : Address;
      Reference : Address)
      return Address;

end Aqua.Arithmetic;
