package Aqua.Options is

   procedure Set_Option
     (Name  : String;
      Value : String);

   function Trace_Code return Boolean;
   function Trace_Link return Boolean;
   function Profile return Boolean;

end Aqua.Options;
