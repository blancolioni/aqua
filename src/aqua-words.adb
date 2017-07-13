package body Aqua.Words is

   -----------------------------
   -- Get_Primitive_Reference --
   -----------------------------

   function Get_Primitive_Reference
     (Value : Word)
      return Primitive_Reference
   is
   begin
      return Primitive_Reference (Value and Payload_Mask);
   end Get_Primitive_Reference;

   ----------------------------
   -- Is_Primitive_Reference --
   ----------------------------

   function Is_Primitive_Reference
     (Value : Word)
      return Boolean
   is
   begin
      return Get_Tag (Value) = Primitive_Tag;
   end Is_Primitive_Reference;

   -----------------------
   -- To_Primitive_Word --
   -----------------------

   function To_Primitive_Word
     (Reference : Primitive_Reference)
      return Word
   is
   begin
      return Set_Tag (Word (Reference), Primitive_Tag);
   end To_Primitive_Word;

end Aqua.Words;
