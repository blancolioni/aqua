private with Ada.Calendar;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Ada.Finalization;

private with Aqua.Architecture;

with Aqua.Execution;
with Aqua.Images;

package Aqua.CPU is

   Halt_Instruction : exception;
   Runtime_Error    : exception;

   type Aqua_CPU_Type is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with private;

   type Aqua_CPU is access all Aqua_CPU_Type'Class;

   function Create_CPU (Image : Aqua.Images.Image_Type) return Aqua_CPU;

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

   procedure Enable_Profiling (Enabled : Boolean);
   procedure Write_Profile (Path : String);

private

   type Saved_Register_Count is mod 8;

   type Saved_Register_Array is array (Saved_Register_Count) of Word;

   type Saved_Registers is
      record
         Count : Saved_Register_Count;
         Rs    : Saved_Register_Array;
      end record;

   package List_Of_Saved_Registers is
     new Ada.Containers.Doubly_Linked_Lists (Saved_Registers);

   type Opcode_Acc_Array is array (Octet) of Natural;
   type Operand_Acc_Array is array (0 .. 15) of Natural;

   type Aqua_CPU_Type is
   limited new Ada.Finalization.Limited_Controlled
     and Aqua.Execution.Execution_Interface with
      record
         Image       : Aqua.Images.Image_Type;
         R           : Aqua.Architecture.Registers :=
                         (Architecture.R_PC => 16#FFFF_FFFC#,
                          Architecture.R_SP => 16#8000_0000#,
                          others            => 16#BAAD_F00D#);
         R_Stack     : List_Of_Saved_Registers.List;
         R_Local     : Aqua.Architecture.Register_Index := 0;
         R_Global    : Aqua.Architecture.Register_Index := 29;
         N, Z, C, V  : Boolean := False;
         B           : Boolean := False;
         Start       : Ada.Calendar.Time;
         Exec_Time   : Duration := 0.0;
         Current_Env : Ada.Strings.Unbounded.Unbounded_String;
         Opcode_Acc  : Opcode_Acc_Array;
         Operand_Acc : Operand_Acc_Array;
      end record;

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

end Aqua.CPU;
