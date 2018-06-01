with Ada.Text_IO;

with Aqua.Architecture;
with Aqua.Drivers.Meta_Driver;
with Aqua.IO;
with Aqua.Options;

package body Aqua.CPU is

   Trace_Code        : Boolean := False;
   Trace_Executions  : constant Boolean := False;

   ---------------
   -- Add_Watch --
   ---------------

   procedure Add_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address)
   is null;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (CPU              : in out Aqua_CPU_Type;
      Environment_Name : String;
      Start            : Address;
      Arguments        : Array_Of_Words)
   is
      use type Ada.Calendar.Time;
      use Ada.Strings.Unbounded;
   begin

      Trace_Code := Aqua.Options.Trace_Code;

      CPU.Opcode_Acc := (others => 0);

      CPU.Set_Current_Environment (Environment_Name);
      CPU.PC := Start;

      if Trace_Code or else Trace_Executions then
         Ada.Text_IO.Put_Line
           ("start: ["
            & Environment_Name
            & "] "
            & Aqua.IO.Hex_Image (Start));
      end if;

      CPU.Start := Ada.Calendar.Clock;

      declare
         R : Register_Stack_Range := 0;
      begin
         for Arg of Arguments loop
            CPU.Stack (R) := Arg;
            R := R + 1;
         end loop;
         CPU.Local := Octet (R);
      end;

      CPU.Global := 255;
      CPU.Gs (255) := Start;

      loop
         if not CPU.Image.Can_Execute (CPU.PC) then
            raise Page_Fault with "pc not executable";
         end if;

         declare
            use Aqua.Architecture;
            IR : constant Word := CPU.Image.Get_Word (CPU.PC);
            Instruction : Aqua_Instruction;
            X, Y, Z     : Octet;
         begin
            Decode (IR, Instruction, X, Y, Z);

            case Instruction is
               when A_TRAP =>
                  exit;
               when A_ADD =>
                  CPU.Set_R (X, CPU.Get_R (Y) + CPU.Get_R (Z));
               when A_ADDI =>
                  CPU.Set_R (X, CPU.Get_R (Y) + Word (Z));
               when A_ADDU =>
                  CPU.Set_R (X, CPU.Get_R (Y) + CPU.Get_R (Z));
               when A_ADDUI =>
                  CPU.Set_R (X, CPU.Get_R (Y) + Word (Z));
               when others =>
                  null;
            end case;

         end;
      end loop;

   end Execute;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      Aqua.Drivers.Meta_Driver.Create_Meta_Driver (CPU.Image);
      Aqua.Drivers.Register
        ("aqua-text-writer", Aqua.Drivers.Text_Writer'Access);
      Aqua.Drivers.Register
        ("aqua-character-handler",
         Aqua.Drivers.Character_Handling'Access);
   end Initialize;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (CPU : Aqua_CPU_Type)
   is
      use Ada.Calendar;
      use Ada.Text_IO;
   begin
      Put_Line
        ("code: "
         & Aqua.IO.Hex_Image (CPU.Image.Code_Base)
         & " .. "
         & Aqua.IO.Hex_Image (CPU.Image.Code_Bound)
         & " size"
         & Word'Image (CPU.Image.Code_Size));
      Put_Line
        ("text: "
         & Aqua.IO.Hex_Image (CPU.Image.Segment_Base ("text"))
         & " .. "
         & Aqua.IO.Hex_Image (CPU.Image.Segment_Bound ("text"))
         & " size"
         & Word'Image (CPU.Image.Segment_Size ("text")));
      Put_Line ("CPU time:"
                & Natural'Image (Natural (CPU.Exec_Time * 1000.0))
                & "ms");
   end Report;

   ------------------
   -- Remove_Watch --
   ------------------

   procedure Remove_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address)
   is null;

   ---------
   -- Run --
   ---------

   procedure Run (CPU : in out Aqua_CPU_Type'Class) is
      No_Arguments : Array_Of_Words (1 .. 0);
   begin
      CPU.Execute
        (Environment_Name => "aqua",
         Start            => CPU.Image.Start_Address,
         Arguments        => No_Arguments);
   end Run;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word
   is (0);

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
   is null;

   -----------------------------
   -- Set_Current_Environment --
   -----------------------------

   procedure Set_Current_Environment
     (CPU  : in out Aqua_CPU_Type'Class;
      Name : String)
   is
   begin
      CPU.Current_Env :=
        Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Current_Environment;

   -----------
   -- Set_R --
   -----------

   procedure Set_R
     (CPU : in out Aqua_CPU_Type'Class;
      R   : Octet;
      X   : Word)
   is
   begin
      if R >= CPU.Global then
         CPU.Gs (R) := X;
      else
         CPU.Stack (CPU.SP + Register_Stack_Range (R)) := X;
         if R >= CPU.Local then
            for I in CPU.Local .. R - 1 loop
               CPU.Stack (CPU.SP + Register_Stack_Range (I)) := 0;
            end loop;
            CPU.Local := R + 1;
         end if;
      end if;
   end Set_R;

   ----------
   -- Show --
   ----------

   overriding function Show
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
      pragma Unreferenced (CPU);
   begin
      return Aqua.IO.Hex_Image (Value);
   end Show;

   --------------------
   -- Show_Registers --
   --------------------

   procedure Show_Registers
     (CPU : in out Aqua_CPU_Type)
   is null;

   ----------------
   -- Show_Stack --
   ----------------

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type)
   is null;

end Aqua.CPU;
