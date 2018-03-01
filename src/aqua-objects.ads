private with WL.String_Maps;

with Aqua.Iterators;
with Aqua.Values;

package Aqua.Objects is

   type Object_Interface is interface
     and External_Object_Interface
     and Aqua.Iterators.Aqua_Container_Interface;

   procedure Set_Property
     (Object : in out Object_Interface;
      Name   : in     String;
      Value  : in     Aqua.Values.Property_Value)
   is abstract;

   function Get_Property
     (Object : in out Object_Interface;
      Name   : in String)
      return Aqua.Values.Property_Value
      is abstract;

   function Has_Property
     (Object : in Object_Interface;
      Name   : in String)
      return Boolean
      is abstract;

   procedure Scan_Properties
     (Object  : in Object_Interface;
      Process : not null access
        procedure (Name  : String;
                   Value : Aqua.Values.Property_Value))
   is abstract;

   procedure Set_Property
     (Object : in out Object_Interface'Class;
      Name   : in     String;
      Value  : in     Word);

   type Object_Access is access all Object_Interface'Class;

   type Root_Object_Type is
     new Object_Interface with private;

   overriding procedure Set_Property
     (Object : in out Root_Object_Type;
      Name   : in     String;
      Value  : in     Aqua.Values.Property_Value);

   overriding function Get_Property
     (Object : in out Root_Object_Type;
      Name   : in String)
      return Aqua.Values.Property_Value;

   overriding function Has_Property
     (Object : in Root_Object_Type;
      Name   : in String)
      return Boolean;

private

   package Object_Maps is
     new WL.String_Maps (Aqua.Values.Property_Value,
                         Aqua.Values."=");

   type Root_Object_Type is
     new Object_Interface with
      record
         Map : Object_Maps.Map;
         Ref : External_Reference := 0;
      end record;

   overriding function Name
     (Object : Root_Object_Type)
      return String
   is ("[object]");

   overriding function Text
     (Object : Root_Object_Type)
      return String
   is ("[object]");

   overriding function Show
     (Object : Root_Object_Type;
      Recursive_Show : access
        function (Value : Word) return String)
      return String;

   overriding function Start
     (Object : Root_Object_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class;

   overriding procedure Set_Reference
     (Object : in out Root_Object_Type;
      Reference : External_Reference);

   overriding function Get_Reference
     (Object : Root_Object_Type)
      return External_Reference;

   overriding procedure Scan_Properties
     (Object  : Root_Object_Type;
      Process : not null access
        procedure (Name  : String;
                   Value : Aqua.Values.Property_Value));

   type Root_Object_Iterator is
     new Aqua.Iterators.Aqua_Iterator_Interface with
      record
         Current  : Aqua.Values.Property_Value;
         Position : Object_Maps.Cursor;
         Ref      : External_Reference := 0;
      end record;

   overriding function Name
     (It : Root_Object_Iterator)
      return String
   is ("[object-iterator]");

   overriding function Text
     (It : Root_Object_Iterator)
      return String
   is ("[object-iterator]");

   overriding function Show
     (It : Root_Object_Iterator;
     Recursive_Show : access
       function (Value : Word) return String)
      return String
   is ("[object-iterator]");

   overriding procedure Set_Reference
     (It : in out Root_Object_Iterator;
      Reference : External_Reference);

   overriding function Get_Reference
     (It : Root_Object_Iterator)
      return External_Reference;

   overriding procedure Next
     (It       : in out Root_Object_Iterator;
      Finished :    out Boolean);

   overriding function Current
     (It : Root_Object_Iterator)
      return Word
   is (Aqua.Values.To_Word (It.Current));

end Aqua.Objects;
