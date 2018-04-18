with Ada.Text_IO;

package body Aqua.Options is

   Local_Trace_Code : Boolean := False;
   Local_Trace_Link : Boolean := False;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option
     (Name  : String;
      Value : String)
   is
   begin
      if Name = "trace-code" then
         Local_Trace_Code := Boolean'Value (Value);
      elsif Name = "trace-link" then
         Local_Trace_Link := Boolean'Value (Value);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Name & ": unknown option");
      end if;
   end Set_Option;

   ----------------
   -- Trace_Code --
   ----------------

   function Trace_Code return Boolean is
   begin
      return Local_Trace_Code;
   end Trace_Code;

   ----------------
   -- Trace_Link --
   ----------------

   function Trace_Link return Boolean is
   begin
      return Local_Trace_Link;
   end Trace_Link;

end Aqua.Options;
