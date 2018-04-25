private with Ada.Calendar;
private with Ada.Strings.Unbounded;

with Ada.Finalization;

with Aqua.Architecture;

with Aqua.Execution;
with Aqua.Images;

package Aqua.CPU is

   Halt_Instruction : exception;
   Runtime_Error    : exception;

   type Aqua_CPU_Type (Image : access Aqua.Images.Root_Image_Type'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface
     and Aqua.Architecture.Architecture_Interface with private;

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

   procedure Run (CPU : in out Aqua_CPU_Type'Class);

private

   Default_Register_Value : constant := 16#BAAD_F00D#;

   Register_Window_Size : constant := 4096;
   type Register_Window_Index is range 0 .. Register_Window_Size - 1;
   type Register_Window is array (Register_Window_Index) of Word;

   type Opcode_Acc_Array is array (Octet) of Natural;
   type Operand_Acc_Array is array (0 .. 15) of Natural;

   type Aqua_CPU_Type (Image : access Aqua.Images.Root_Image_Type'Class) is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface
     and Aqua.Architecture.Architecture_Interface with
      record
         Globals     : Aqua.Architecture.Registers :=
                         (Architecture.R_PC => 16#FFFF_FFFC#,
                          Architecture.R_SP => 16#8000_0000#,
                          others            => Default_Register_Value);
         Window      : Register_Window := (others => 16#BAAD_F00D#);
         Zero        : Register_Window_Index := Register_Window_Index'First;
         R_Local     : Aqua.Architecture.Register_Index := 0;
         R_Global    : Aqua.Architecture.Register_Index := 29;
         R_Jump      : Word := Default_Register_Value;
         N, Z, C, V  : Boolean := False;
         B           : Boolean := False;
         Start       : Ada.Calendar.Time;
         Exec_Time   : Duration := 0.0;
         Current_Env : Ada.Strings.Unbounded.Unbounded_String;
         Opcode_Acc  : Opcode_Acc_Array;
         Operand_Acc : Operand_Acc_Array;
      end record;

   function Window_Index
     (CPU : Aqua_CPU_Type'Class;
      R   : Aqua.Architecture.Register_Index)
      return Register_Window_Index
   is (CPU.Zero + Register_Window_Index (R));

   function Window_Local
     (CPU : Aqua_CPU_Type'Class)
      return Register_Window_Index
   is (CPU.Window_Index (CPU.R_Local));

   overriding function Next_Value
     (CPU : in out Aqua_CPU_Type;
      Size : Data_Size)
      return Word;

   overriding procedure Set_R
     (CPU : in out Aqua_CPU_Type;
      R   : Aqua.Architecture.Register_Index;
      X   : Word);

   overriding function Get_R
     (CPU : in out Aqua_CPU_Type;
      R   : Aqua.Architecture.Register_Index)
      return Word;

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word
   is (0);

   overriding procedure Push
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
   is null;

   function Next_Octet
     (CPU : in out Aqua_CPU_Type'Class)
      return Octet
   is (Octet (Next_Value (CPU, Word_8_Size)));

   function Next_Operand
     (CPU : in out Aqua_CPU_Type'Class)
      return Aqua.Architecture.Operand_Type;

   procedure Show_Registers
     (CPU : in out Aqua_CPU_Type);

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type);

   overriding function Environment_Name
     (CPU   : Aqua_CPU_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (CPU.Current_Env));

end Aqua.CPU;
