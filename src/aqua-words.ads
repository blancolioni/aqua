package Aqua.Words is

   function Is_Primitive_Reference
     (Value : Word)
      return Boolean;

   function Get_Primitive_Reference
     (Value : Word)
      return Primitive_Reference
     with Pre => Is_Primitive_Reference (Value);

   function To_Primitive_Word
     (Reference : Primitive_Reference)
      return Word;

end Aqua.Words;
