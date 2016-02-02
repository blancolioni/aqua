package Aqua.Words is

   function Is_Subroutine_Reference
     (Value : Word)
      return Boolean;

   function Get_Subroutine_Reference
     (Value : Word)
      return Subroutine_Reference
     with Pre => Is_Subroutine_Reference (Value);

   function To_Subroutine_Word
     (Reference : Subroutine_Reference)
      return Word;

end Aqua.Words;
