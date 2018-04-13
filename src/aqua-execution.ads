package Aqua.Execution is

   Execution_Error : exception;

   type Execution_Interface is limited interface;

   function Environment_Name
     (Context : Execution_Interface)
      return String
      is abstract;

   procedure Execute (Context          : in out Execution_Interface;
                      Environment_Name : String;
                      Start            : Address;
                      Arguments        : Array_Of_Words)
   is abstract;

   function Show (Context : in out Execution_Interface;
                  Value   : Word)
                  return String
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

end Aqua.Execution;
