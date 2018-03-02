private with Ada.Containers.Doubly_Linked_Lists;

with Aqua.Values;

package Aqua.Objects.Lists is

   type Root_List_Type is
     new Root_Object_Type with private;

   overriding procedure Set_Property
     (Object : in out Root_List_Type;
      Name   : in     String;
      Value  : in     Aqua.Values.Property_Value)
   is null;

   overriding function Get_Property
     (Object : in out Root_List_Type;
      Name   : in String)
      return Aqua.Values.Property_Value
   is (Aqua.Values.To_Word_Value (0));

   overriding function Has_Property
     (Object : in Root_List_Type;
      Name   : in String)
      return Boolean
   is (False);

   procedure Append
     (Object : in out Root_List_Type;
      Value  : Word);

private

   package Object_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Word);

   type Root_List_Type is
     new Root_Object_Type with
      record
         List : Object_Lists.List;
      end record;

   overriding function Name
     (Object : Root_List_Type)
      return String
   is ("[list]");

   overriding function Text
     (Object : Root_List_Type)
      return String
   is ("[list]");

   overriding function Show
     (Object         : Root_List_Type;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String;

   overriding function Start
     (Object : Root_List_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class;

   overriding procedure Scan_Properties
     (Object   : Root_List_Type;
      Process  : not null access
        procedure (Name : String;
                   Value : Aqua.Values.Property_Value));

   type Root_List_Iterator is
     new Aqua.Iterators.Aqua_Iterator_Interface with
      record
         Current  : Word;
         Position : Object_Lists.Cursor;
         Ref      : External_Reference := 0;
      end record;

   overriding function Name
     (It : Root_List_Iterator)
      return String
   is ("[list-iterator]");

   overriding function Class_Name
     (It : Root_List_Iterator)
      return String
   is ("[list-iterator]");

   overriding function Text
     (It : Root_List_Iterator)
      return String
   is ("[list-iterator]");

   overriding function Show
     (It             : Root_List_Iterator;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String
   is ("[list-iterator]");

   overriding procedure Set_Reference
     (It        : in out Root_List_Iterator;
      Reference : External_Reference);

   overriding function Get_Reference
     (It : Root_List_Iterator)
      return External_Reference
   is (It.Ref);

   overriding procedure Next
     (It       : in out Root_List_Iterator;
      Finished :    out Boolean);

   overriding function Current
     (It : Root_List_Iterator)
      return Word
   is (It.Current);

end Aqua.Objects.Lists;
