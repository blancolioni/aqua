private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;

with Aqua.Iterators;

package Aqua.Objects is

   type Object_Interface is interface
     and External_Object_Interface
     and Aqua.Iterators.Aqua_Container_Interface;

   procedure Set_Property
     (Object : in out Object_Interface;
      Name   : in     String;
      Value  : in     Word)
   is abstract;

   function Get_Property
     (Object : in out Object_Interface;
      Name   : in String)
      return Word
      is abstract;

   function Has_Property
     (Object : in Object_Interface;
      Name   : in String)
      return Boolean
      is abstract;

   type Root_Object_Type is
     new Object_Interface with private;

   overriding procedure Set_Property
     (Object : in out Root_Object_Type;
      Name   : in     String;
      Value  : in     Word);

   overriding function Get_Property
     (Object : in out Root_Object_Type;
      Name   : in String)
      return Word;

   overriding function Has_Property
     (Object : in Root_Object_Type;
      Name   : in String)
      return Boolean;

private

   package Object_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Word,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Root_Object_Type is
     new Object_Interface with
      record
         Map : Object_Maps.Map;
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
     (Object : Root_Object_Type)
      return String;

   overriding function Start
     (Object : Root_Object_Type)
      return Aqua.Iterators.Aqua_Iterator_Interface'Class;

   type Root_Object_Iterator is
     new Aqua.Iterators.Aqua_Iterator_Interface with
      record
         Current  : Word;
         Position : Object_Maps.Cursor;
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
     (It : Root_Object_Iterator)
      return String
   is ("[object-iterator]");

   overriding procedure Next
     (It       : in out Root_Object_Iterator;
      Finished :    out Boolean);

   overriding function Current
     (It : Root_Object_Iterator)
      return Word
   is (It.Current);

end Aqua.Objects;
