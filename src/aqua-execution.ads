package Aqua.Execution is

   Execution_Error : exception;

   type Execution_Interface is limited interface;

   procedure Execute (Context   : in out Execution_Interface;
                      Start     : Address;
                      Arguments : Array_Of_Words)
   is abstract;

   function Show (Context : in out Execution_Interface;
                  Value   : Word)
                  return String
                  is abstract;

   function To_String_Word
     (Context : in out Execution_Interface;
      Value   : String)
      return Word
      is abstract;

   function To_String
     (Context : in out Execution_Interface;
      Value : Word)
      return String
   is abstract
     with Pre'Class => Is_Integer (Value)
     or else Is_String_Reference (Value);

   function To_Integer
     (Context : in out Execution_Interface;
      Value : Word)
      return Aqua_Integer
   is abstract
     with Pre'Class => Is_Integer (Value)
     or else Is_String_Reference (Value);

   function To_External_Object
     (Context : in out Execution_Interface;
      Value : Word)
      return access External_Object_Interface'Class
   is abstract
     with Pre'Class => Is_External_Reference (Value);

   function To_Word
     (Context : in out Execution_Interface;
      Item : not null access External_Object_Interface'Class)
      return Word
      is abstract;

   function Pop
     (Context : in out Execution_Interface)
      return Word
      is abstract;

   procedure Push
     (Context : in out Execution_Interface;
      Value   : Word)
   is abstract;

   procedure Report
     (Context : Execution_Interface)
   is abstract;

   type Loader_Interface is interface;

   function Load_Object (Loader    : in out Loader_Interface;
                         File_Name : String)
                         return access External_Object_Interface'Class
                         is abstract;

   function Loader
     (Context : Execution_Interface)
      return access Loader_Interface'Class
      is abstract;

end Aqua.Execution;
