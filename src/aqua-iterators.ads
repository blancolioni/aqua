package Aqua.Iterators is

   type Aqua_Iterator_Interface is interface and External_Object_Interface;

   procedure Next (Iterator : in out Aqua_Iterator_Interface;
                   Finished :    out Boolean) is abstract;
   function Current (Iterator : Aqua_Iterator_Interface) return Word
                     is abstract;

   type Aqua_Container_Interface is interface and External_Object_Interface;

   function Start (Container : Aqua_Container_Interface)
                   return Aqua_Iterator_Interface'Class
                   is abstract;

end Aqua.Iterators;
