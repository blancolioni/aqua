package body Aqua.Words is

   ------------------------------
   -- Get_Subroutine_Reference --
   ------------------------------

   function Get_Subroutine_Reference
     (Value : Word)
      return Subroutine_Reference
   is
   begin
      return Subroutine_Reference (Value and Payload_Mask);
   end Get_Subroutine_Reference;

   -----------------------------
   -- Is_Subroutine_Reference --
   -----------------------------

   function Is_Subroutine_Reference
     (Value : Word)
      return Boolean
   is
   begin
      return Get_Tag (Value) = Subroutine_Tag;
   end Is_Subroutine_Reference;

   ------------------------
   -- To_Subroutine_Word --
   ------------------------

   function To_Subroutine_Word
     (Reference : Subroutine_Reference)
      return Word
   is
   begin
      return Set_Tag (Word (Reference), Subroutine_Tag);
   end To_Subroutine_Word;

end Aqua.Words;
