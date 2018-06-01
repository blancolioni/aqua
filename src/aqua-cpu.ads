private with Ada.Calendar;
private with Ada.Strings.Unbounded;

with Ada.Finalization;

with Aqua.Execution;
with Aqua.Images;

package Aqua.CPU is

   Halt_Instruction : exception;
   Runtime_Error    : exception;
   Page_Fault       : exception;

   type Aqua_CPU_Type (Image : access Aqua.Images.Root_Image_Type'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with private;

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type);

   overriding procedure Finalize
     (CPU : in out Aqua_CPU_Type);

   overriding procedure Execute
     (CPU              : in out Aqua_CPU_Type;
      Environment_Name : String;
      Start            : Address;
      Arguments        : Array_Of_Words);

   overriding function Show
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String;

   overriding function Environment_Name
     (CPU   : Aqua_CPU_Type)
      return String;

   overriding procedure Report
     (CPU : Aqua_CPU_Type);

   procedure Set_Current_Environment
     (CPU  : in out Aqua_CPU_Type'Class;
      Name : String);

   procedure Add_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address);

   procedure Remove_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address);

   procedure Run (CPU : in out Aqua_CPU_Type'Class);

private

   Register_Stack_Length : constant := 4096;

   type Register_Stack_Range is range 0 .. Register_Stack_Length - 1;

   type Register_Stack_Array is array (Register_Stack_Range) of Word;

   type Global_Register_Array is array (Octet) of Word;

   type Opcode_Acc_Array is array (Octet) of Natural;

   type Aqua_CPU_Type (Image : access Aqua.Images.Root_Image_Type'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with
      record
         Stack       : Register_Stack_Array := (others => 0);
         Gs          : Global_Register_Array := (others => 0);
         SP          : Register_Stack_Range := 0;
         Local       : Octet := 0;
         Global      : Octet := 255;
         Jump        : Address := 0;
         PC          : Address := 0;
         N, Z, C, V  : Boolean := False;
         B           : Boolean := False;
         Start       : Ada.Calendar.Time;
         Exec_Time   : Duration := 0.0;
         Current_Env : Ada.Strings.Unbounded.Unbounded_String;
         Opcode_Acc  : Opcode_Acc_Array;
      end record;

   procedure Set_R
     (CPU : in out Aqua_CPU_Type'Class;
      R   : Octet;
      X   : Word);

   function Get_R
     (CPU : in out Aqua_CPU_Type'Class;
      R   : Octet)
      return Word;

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word;

   overriding procedure Push
     (CPU : in out Aqua_CPU_Type;
      Value : Word);

   procedure Show_Registers
     (CPU : in out Aqua_CPU_Type);

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type);

   overriding function Environment_Name
     (CPU   : Aqua_CPU_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (CPU.Current_Env));

   function Get_R
     (CPU : in out Aqua_CPU_Type'Class;
      R   : Octet)
      return Word
   is (if R >= CPU.Global then CPU.Gs (R)
       elsif R >= CPU.Local then 0
       else CPU.Stack (CPU.SP + Register_Stack_Range (R)));

end Aqua.CPU;
