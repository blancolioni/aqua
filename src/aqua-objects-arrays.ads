with Ada.Containers.Vectors;

package Aqua.Objects.Arrays is

   type Root_Array_Type is
     new Object_Interface with private;

   overriding procedure Set_Property
     (Object : in out Root_Array_Type;
      Name   : in     String;
      Value  : in     Word);

   overriding function Get_Property
     (Object : in out Root_Array_Type;
      Name   : in String)
      return Word;

   overriding function Has_Property
     (Object : in Root_Array_Type;
      Name   : in String)
      return Boolean;

   procedure Append
     (Object : in out Root_Array_Type;
      Value  : Word);

   function Last_Index
     (Object : Root_Array_Type)
     return Aqua_Integer;

   function Get_Element
     (Object : Root_Array_Type;
      Index  : Aqua_Integer)
      return Word
     with Pre => Index in 1 .. Object.Last_Index;

private

   package Object_Vectors is
     new Ada.Containers.Vectors (Positive, Word);

   type Root_Array_Type is
     new Object_Interface with
      record
         Vector : Object_Vectors.Vector;
         Ref    : External_Reference := 0;
      end record;

   overriding function Name
     (Object : Root_Array_Type)
      return String
   is ("[array]");

   overriding function Text
     (Object : Root_Array_Type)
      return String
   is ("[array]");

   overriding function Show
     (Object : Root_Array_Type)
      return String;

   overriding function Start
     (Object : Root_Array_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class;

   overriding procedure Set_Reference
     (Object : in out Root_Array_Type;
      Reference : External_Reference);

   overriding function Get_Reference
     (Object : Root_Array_Type)
      return External_Reference
   is (Object.Ref);

   type Root_Array_Iterator is
     new Aqua.Iterators.Aqua_Iterator_Interface with
      record
         Current  : Word;
         Position : Object_Vectors.Cursor;
         Ref      : External_Reference := 0;
      end record;

   overriding function Name
     (It : Root_Array_Iterator)
      return String
   is ("[array-iterator]");

   overriding function Text
     (It : Root_Array_Iterator)
      return String
   is ("[array-iterator]");

   overriding function Show
     (It : Root_Array_Iterator)
      return String
   is ("[array-iterator]");

   overriding procedure Set_Reference
     (It        : in out Root_Array_Iterator;
      Reference : External_Reference);

   overriding function Get_Reference
     (It : Root_Array_Iterator)
      return External_Reference
   is (It.Ref);

   overriding procedure Next
     (It       : in out Root_Array_Iterator;
      Finished :    out Boolean);

   overriding function Current
     (It : Root_Array_Iterator)
      return Word
   is (It.Current);

end Aqua.Objects.Arrays;
