package body Aqua.Execution is

   -----------------------
   -- To_Property_Value --
   -----------------------

   function To_Property_Value
     (Context : in out Execution_Interface'Class;
      Value   : Word)
      return Aqua.Values.Property_Value
   is
   begin
      if Is_String_Reference (Value) then
         return Aqua.Values.To_String_Value
           (Context.To_String (Value));
      else
         return Aqua.Values.To_Word_Value (Value);
      end if;
   end To_Property_Value;

   -------------
   -- To_Word --
   -------------

   function To_Word
     (Context : in out Execution_Interface'Class;
      Value   : Aqua.Values.Property_Value)
      return Word
   is
   begin
      if Aqua.Values.Has_String (Value) then
         return Context.To_String_Word (Aqua.Values.To_String (Value));
      else
         return Aqua.Values.To_Word (Value);
      end if;
   end To_Word;

end Aqua.Execution;
