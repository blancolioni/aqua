private with Ada.Strings.Unbounded;

package Aqua.Values is

   type Property_Value is private;

   function Has_Word (Value : Property_Value) return Boolean;
   function Has_String (Value : Property_Value) return Boolean;

   function To_String (Value : Property_Value) return String;
   function To_Word   (Value : Property_Value) return Word;

   function Null_Value return Property_Value;

   function To_Word_Value
     (Value : Word)
      return Property_Value;

   function To_String_Value
     (Value : String)
      return Property_Value;

private

   type Property_Value_Contents is (Word_Contents, String_Contents);

   type Property_Value is
      record
         Contents     : Property_Value_Contents;
         Word_Value   : Word;
         String_Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Has_Word (Value : Property_Value) return Boolean
   is (Value.Contents = Word_Contents);

   function Has_String (Value : Property_Value) return Boolean
   is (Value.Contents = String_Contents);

   function To_String (Value : Property_Value) return String
   is (case Value.Contents is
          when Word_Contents =>
             Word'Image (Value.Word_Value),
          when String_Contents =>
             Ada.Strings.Unbounded.To_String (Value.String_Value));

   function To_Word   (Value : Property_Value) return Word
   is (Value.Word_Value);

   function Null_Value return Property_Value
   is (Word_Contents, 0, Ada.Strings.Unbounded.Null_Unbounded_String);

   function To_Word_Value
     (Value : Word)
      return Property_Value
   is (Word_Contents, Value, Ada.Strings.Unbounded.Null_Unbounded_String);

   function To_String_Value
     (Value : String)
      return Property_Value
   is (String_Contents, 0, Ada.Strings.Unbounded.To_Unbounded_String (Value));

end Aqua.Values;
