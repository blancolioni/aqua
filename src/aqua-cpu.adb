with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;

with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Aqua.Debug;
with Aqua.IO;
with Aqua.Traps;

with Aqua.Options;
with Ada.Strings.Fixed;

package body Aqua.CPU is

   use Aqua.Architecture;

   Trace_Code       : Boolean := False;
   Trace_Window     : Boolean := False;
   Trace_Calls      : constant Boolean := True;
   Trace_Executions : constant Boolean := False;

   type Branch_Info is
      record
         Condition : Condition_Code;
         Asserted  : Boolean;
      end record;

   Branch_Info_Array : constant array (Branch_Instruction) of Branch_Info :=
                         (A_Br => (Always, True),
                          A_Bne => (EQ, False),
                          A_Beq => (EQ, True),
                          A_Bge => (LT, False),
                          A_Blt => (LT, True),
                          A_Bgt => (LE, False),
                          A_Ble => (LE, True),
                          A_Bpl => (MI, False),
                          A_Bmi => (MI, True),
                          A_Bhi => (LOS, False),
                          A_Blos => (LOS, True),
                          A_Bvc => (VS, False),
                          A_Bvs => (VS, True),
                          A_Bcc => (CS, False),
                          A_Bcs => (CS, True));

   type Double_Operand_Handler is access
     procedure (CPU  : in out Aqua_CPU_Type'Class;
                Size : Aqua.Data_Size;
                Src  : Aqua.Word;
                Dst  : in out Aqua.Word);

   type Single_Operand_Handler is access
     procedure (CPU  : in out Aqua_CPU_Type'Class;
                Size : Aqua.Data_Size;
                Dst  : in out Aqua.Word);

   function Convert_Triple_To_Double
     (Triple : Triple_Operand_Instruction)
      return Double_Operand_Instruction;

   procedure Handle_Mov
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Cmp
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Add
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Div
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Mul
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Sub
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_And
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Or
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Xor
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word);

   procedure Handle_Clr
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Dec
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Inc
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Ldj
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Stj
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   Double_Operand : constant array (Double_Operand_Instruction)
     of Double_Operand_Handler :=
       (A_Mov  => Handle_Mov'Access,
        A_Cmp  => Handle_Cmp'Access,
        A_Add  => Handle_Add'Access,
        A_Div  => Handle_Div'Access,
        A_Sub  => Handle_Sub'Access,
        A_Mul  => Handle_Mul'Access,
        A_And  => Handle_And'Access,
        A_Or   => Handle_Or'Access,
        A_Xor  => Handle_Xor'Access,
        others => null);

   Single_Operand : constant array (Single_Operand_Instruction)
     of Single_Operand_Handler :=
       (A_Clr => Handle_Clr'Access,
        A_Inc => Handle_Inc'Access,
        A_Dec => Handle_Dec'Access,
        A_Tst => null,
        A_Ldj => Handle_Ldj'Access,
        A_Stj => Handle_Stj'Access,
        others => null);

   procedure Handle
     (CPU : in out Aqua_CPU_Type'Class;
      Op  : in     Octet);

   procedure Handle_Branch
     (CPU         : in out Aqua_CPU_Type'Class;
      Condition   : Aqua.Architecture.Condition_Code;
      Negate      : Boolean;
      Offset      : Word);

   procedure Handle_Trap
     (CPU   : in out Aqua_CPU_Type'Class;
      Index : Natural);

   procedure Set_NZ
     (CPU   : in out Aqua_CPU_Type'Class;
      Size  : Data_Size;
      Value : Word)
     with Inline_Always;

   procedure Dump_Core
     (CPU : in out Aqua_CPU_Type'Class);

   ------------------------------
   -- Convert_Triple_To_Double --
   ------------------------------

   function Convert_Triple_To_Double
     (Triple : Triple_Operand_Instruction)
      return Double_Operand_Instruction
   is
   begin
      case Triple is
         when A_Add_3 =>
            return A_Add;
         when A_And_3 =>
            return A_And;
         when A_Div_3 =>
            return A_Div;
         when A_Mul_3 =>
            return A_Mul;
         when A_Or_3 =>
            return A_Or;
         when A_Sub_3 =>
            return A_Sub;
         when A_Xor_3 =>
            return A_Xor;
      end case;
   end Convert_Triple_To_Double;

   ---------------
   -- Dump_Core --
   ---------------

   procedure Dump_Core
     (CPU : in out Aqua_CPU_Type'Class)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, "aqua-core.txt");
      Set_Output (File);

      CPU.Show_Registers;
      CPU.Report;

      Set_Output (Standard_Output);
      Close (File);
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unabled to create core file");
   end Dump_Core;

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
      Last_Source : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Trace_Code := Aqua.Options.Trace_Code;
      Trace_Window := Aqua.Options.Trace_Code;

      CPU.Opcode_Acc := (others => 0);
      CPU.Operand_Acc := (others => 0);

      CPU.Set_Current_Environment (Environment_Name);

      if Trace_Code or else Trace_Executions then
         Ada.Text_IO.Put_Line
           ("start: ["
            & Environment_Name
            & "]");
      end if;

      CPU.Start := Ada.Calendar.Clock;
      CPU.Zero := Register_Window_Index'First;
      CPU.R_Local := Register_Index (CPU.Zero);

      declare
         R : Register_Index := 0;
      begin
         for Arg of Arguments loop
            CPU.Set_R (R, Arg);
            R := R + 1;
         end loop;
      end;

      CPU.Globals (R_PC) := Start;
      CPU.B := False;

      while not CPU.B loop
         declare
            Op : constant Octet := CPU.Next_Octet;
            Original_PC : constant Word := CPU.Globals (R_PC);
         begin
            CPU.Opcode_Acc (Op) := CPU.Opcode_Acc (Op) + 1;
            if Trace_Code then
               declare
                  Loc : constant String :=
                          CPU.Image.Show_Source_Position
                            (Original_PC);
               begin
                  if Last_Source /= Loc then
                     Last_Source := To_Unbounded_String (Loc);
                     if Loc'Length < 16 then
                        Ada.Text_IO.Put (Loc);
                     else
                        Ada.Text_IO.Put
                          ("..." & Loc (Loc'Last - 12 .. Loc'Last));
                     end if;
                  end if;
               end;
               Ada.Text_IO.Set_Col (20);
               if Trace_Window then
                  Ada.Integer_Text_IO.Put (Natural (CPU.Zero), 4);
                  Ada.Integer_Text_IO.Put (Natural (CPU.Window_Local), 5);
                  Ada.Integer_Text_IO.Put (Natural (CPU.R_Local), 3);
                  Ada.Integer_Text_IO.Put (Natural (CPU.R_Global), 3);
                  Ada.Text_IO.Put (" ");
               end if;

               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Original_PC)
                  & ": ");

               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Op)
                  & " "
                  & Aqua.Debug.Opcode_Image (Op));

            end if;

            begin
               CPU.Image.Begin_Transaction;
               Handle (CPU, Op);
               CPU.Image.End_Transaction;

            exception
               when E : Runtime_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     CPU.Image.Show_Source_Position
                       (Original_PC)
                     & ": error: "
                     & Ada.Exceptions.Exception_Message (E));

                  CPU.Dump_Core;
                  CPU.B := True;
                  CPU.Report;
                  Ada.Text_IO.Put_Line ("core dumped");
                  raise;
            end;

         end;
      end loop;

      CPU.Exec_Time := CPU.Exec_Time + Ada.Calendar.Clock - CPU.Start;

      declare
         type Opcode_Entry is
            record
               Opcode    : Octet;
               Frequency : Natural;
            end record;

         function "<" (Left, Right : Opcode_Entry) return Boolean
         is (Left.Frequency < Right.Frequency);

         package Opcode_Frequency_Tables is
           new Ada.Containers.Doubly_Linked_Lists (Opcode_Entry);

         package Sorted_Opcodes is
           new Opcode_Frequency_Tables.Generic_Sorting ("<");

         Freq : Opcode_Frequency_Tables.List;
         Total : Natural := 0;
      begin
         for Opcode in CPU.Opcode_Acc'Range loop
            if CPU.Opcode_Acc (Opcode) > 0 then
               Freq.Append ((Opcode, CPU.Opcode_Acc (Opcode)));
               Total := Total + CPU.Opcode_Acc (Opcode);
            end if;
         end loop;

         Sorted_Opcodes.Sort (Freq);

         for Item of Freq loop
            Ada.Text_IO.Put
              (Aqua.Debug.Opcode_Image (Item.Opcode));
            Ada.Text_IO.Set_Col (20);
            Ada.Text_IO.Put
              (Natural'Image (Item.Frequency));
            Ada.Text_IO.Set_Col (30);
            Ada.Text_IO.Put
              (Natural'Image (Item.Frequency * 100 / Total) & "%");
            Ada.Text_IO.New_Line;
         end loop;
         Ada.Text_IO.Put ("TOTAL");
         Ada.Text_IO.Set_Col (20);
         Ada.Text_IO.Put (Natural'Image (Total));
         Ada.Text_IO.New_Line;
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("OPERAND             Op        (Op)");

         Ada.Text_IO.Put ("Small integers");
         Ada.Text_IO.Set_Col (20);
         Ada.Text_IO.Put_Line
           (Natural'Image (CPU.Operand_Acc (0) + CPU.Opcode_Acc (1)));

         declare
            procedure Put (Op_Name  : String;
                           Op_Index : Natural;
                           Defer    : Boolean := False);

            ---------
            -- Put --
            ---------

            procedure Put (Op_Name  : String;
                           Op_Index : Natural;
                           Defer    : Boolean := False)
            is
            begin
               Ada.Text_IO.Put (Op_Name);
               Ada.Text_IO.Set_Col (20);
               Ada.Text_IO.Put
                 (Natural'Image (CPU.Operand_Acc (Op_Index)));
               if Defer then
                  Ada.Text_IO.Set_Col (30);
                  Ada.Text_IO.Put
                    (Natural'Image (CPU.Operand_Acc (Op_Index + 5)));
               end if;
               Ada.Text_IO.New_Line;
            end Put;

         begin
            Put ("register", 1, True);
            Put ("X/8(R)", 2, True);
            Put ("(R)+", 3);
            Put ("-(R)", 4);
            Put ("X/32(R)", 5);
         end;

      end;
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

   -----------
   -- Get_R --
   -----------

   overriding function Get_R
     (CPU : in out Aqua_CPU_Type;
      R   : Aqua.Architecture.Register_Index)
      return Word
   is
   begin
      if R >= CPU.R_Global then
         return CPU.Globals (R);
      else
         if R >= CPU.R_Local then
            for I in CPU.R_Local .. R loop
               CPU.Window (CPU.Window_Index (I)) := 0;
            end loop;
            CPU.R_Local := R + 1;
         end if;
         return CPU.Window (CPU.Window_Index (R));
      end if;
   end Get_R;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (CPU : in out Aqua_CPU_Type'Class;
      Op  : in     Octet)
   is
      Instruction : constant Aqua_Instruction := Get_Instruction (Op);
   begin
      case Instruction is
         when A_Halt =>
            CPU.Dump_Core;
            CPU.Report;
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "HALT; core dumped");

            declare
               Addr : Address := CPU.Get_R (0);
            begin
               if CPU.Image.Can_Read (Addr) then
                  declare
                     Length : constant Natural :=
                                Natural
                                  (Word'Min
                                     (1024,
                                      CPU.Image.Get_Word (Addr)));
                     S      : String (1 .. Length) := (others => ' ');
                  begin
                     Addr := Addr + 4;
                     for I in 1 .. Length loop
                        exit when not CPU.Image.Can_Read (Addr);
                        S (I) :=
                          Character'Val (CPU.Image.Get_Word (Addr) mod 256);
                        Addr := Addr + 4;
                     end loop;

                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Message: " & S);
                  end;
               end if;
            end;

            CPU.B := True;

         when A_Nop =>
            null;
         when A_Rts =>
            CPU.Globals (R_PC) := CPU.R_Jump;

         when A_Call =>
            declare
               N    : constant Octet := Next_Octet (CPU);
               Last : Register_Index := Register_Index (N mod 32);
               Dst  : constant Operand_Type := Next_Operand (CPU);
               X    : Word;
            begin

               if Trace_Code then
                  Ada.Text_IO.Put (N'Img);
               end if;

               if Dst.Mode = Register and then not Dst.Deferred then
                  X := CPU.Get_R (Dst.Register);
               else
                  X :=
                    CPU.Get_Address
                      (Operand => Dst,
                       Size    => Word_32_Size,
                       Trace   => Trace_Code,
                       Memory  => CPU.Image.all);
               end if;

               CPU.R_Jump := CPU.Globals (R_PC);
               CPU.Globals (R_PC) := X;

               if Last >= CPU.R_Global then
                  Last := CPU.R_Local;
               elsif Last >= CPU.R_Local then
                  for I in Register_Index range CPU.R_Local .. Last loop
                     CPU.Set_R (I, 0);
                  end loop;
                  CPU.R_Local := Last + 1;
               end if;

               CPU.Set_R (Last, Word (Last));
               CPU.Zero := CPU.Zero + Register_Window_Index (Last) + 1;
               CPU.R_Local := CPU.R_Local - Last - 1;

            end;

         when A_Return =>
            declare
               N    : constant Octet := CPU.Next_Octet;
               Last : Register_Index :=
                        Register_Index (N mod Register_Count);
               P    : constant Register_Index :=
                        Register_Index
                          (CPU.Window (CPU.Zero - 1) mod Register_Count);
            begin

               if Trace_Code then
                  Ada.Text_IO.Put (Last'Img & P'Img);
               end if;

               if Last > CPU.R_Local then
                  Last := CPU.R_Local + 1;
               end if;

               CPU.Window (CPU.Zero - 1) :=
                 (if Last > 0 then CPU.Get_R (Last - 1) else 0);

               CPU.R_Local := Register_Index'Min (P + Last, CPU.R_Global);

               declare
                  Old_X : constant Register_Window_Index :=
                            Register_Window_Index (P);
                  New_L : constant Register_Window_Index :=
                            Register_Window_Index (CPU.R_Local);
                  Dst_Lo : constant Register_Window_Index :=
                             CPU.Zero;
                  Dst_Hi : constant Register_Window_Index :=
                             Dst_Lo + Old_X + New_L - 1;
                  Src_Lo : constant Register_Window_Index :=
                             CPU.Zero - Old_X - 1;
                  Src_Hi : constant Register_Window_Index :=
                             CPU.Zero + New_L - Old_X - 2;
               begin
                  CPU.Window (Dst_Lo .. Dst_Hi) :=
                    CPU.Window (Src_Lo .. Src_Hi);
                  CPU.Zero := CPU.Zero - Old_X - 1;
               end;
            end;

            CPU.Globals (R_PC) := CPU.R_Jump;

         when Single_Operand_Instruction =>
            declare
               Size : constant Data_Size := Get_Size (Op);
               Dst  : Operand_Type;
               A    : Address;
               X    : Word;
            begin
               Dst := Next_Operand (CPU);

               if Instruction = A_Tst then
                  CPU.Read
                    (Dst, Size, Trace_Code,
                     CPU.Image.all, X);
               else
                  if Dst.Mode = Register then
                     A := 0;
                     X := Aqua.Get (CPU.Get_R (Dst.Register), Size);
                  elsif Dst.Mode = Small_Immediate then
                     A := 0;
                     X := Word (Dst.Lit);
                  else
                     A := CPU.Get_Address (Dst, Size, Trace_Code,
                                           CPU.Image.all);
                     X := CPU.Image.Get_Value (A, Size);
                  end if;
               end if;

               if Instruction /= A_Tst then
                  Single_Operand (Instruction) (CPU, Size, X);
               end if;

               Set_NZ (CPU, Size, X);

               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (X));
               end if;

               if Instruction = A_Tst then
                  null;
               elsif Dst.Mode = Register then
                  CPU.Set_R (Dst.Register, Size, X);
               elsif Dst.Mode = Small_Immediate then
                  raise Aqua.Architecture.Bad_Instruction;
               else
                  CPU.Image.Set_Value (A, Size, X);
               end if;
            end;
         when Double_Operand_Instruction =>
            declare
               Size : constant Data_Size := Get_Size (Op);
               Src  : Operand_Type;
               Dst  : Operand_Type;
               A    : Address;
               X, Y : Word;
            begin
               Src := Next_Operand (CPU);
               CPU.Read
                 (Src, Size, Trace_Code,
                  CPU.Image.all, X);

               Dst := Next_Operand (CPU);

               if Dst.Mode /= Small_Immediate
                 and then Dst.Register > CPU.R_Local
                 and then Dst.Register < CPU.R_Global
               then
                  CPU.Set_R (Dst.Register, 0);
                  CPU.R_Local := Dst.Register;
               end if;

               if Instruction = A_Cmp then
                  CPU.Read
                    (Dst, Size, Trace_Code,
                     CPU.Image.all, Y);
               else
                  if Dst.Mode = Register then
                     A := 0;
                     Y := Aqua.Get (CPU.Get_R (Dst.Register), Size);
                  elsif Dst.Mode = Small_Immediate then
                     A := 0;
                     Y := Word (Dst.Lit);
                  else
                     A :=
                       CPU.Get_Address
                         (Dst, Size, Trace_Code,
                          CPU.Image.all);
                     Y := CPU.Image.Get_Value (A, Size);
                  end if;
               end if;

               Double_Operand (Instruction) (CPU, Size, X, Y);

               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Y));
               end if;

               Set_NZ (CPU, Size, Y);

               if Instruction = A_Cmp then
                  null;
               elsif Dst.Mode = Register then
                  CPU.Set_R (Dst.Register, Size, Y);
               elsif Dst.Mode = Small_Immediate then
                  raise Aqua.Architecture.Bad_Instruction;
               else
                  CPU.Image.Set_Value (A, Size, Y);
               end if;
            end;
         when Triple_Operand_Instruction =>
            declare
               Size         : constant Data_Size := Get_Size (Op);
               Src_1, Src_2 : Operand_Type;
               Dst          : Operand_Type;
               X, Y         : Word;
            begin
               Src_1 := Next_Operand (CPU);

               CPU.Read
                 (Src_1, Size, Trace_Code,
                  CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               CPU.Read
                 (Src_2, Size, Trace_Code,
                  CPU.Image.all, Y);

               Dst := Next_Operand (CPU);

               Double_Operand (Convert_Triple_To_Double (Instruction))
                 (CPU, Size, X, Y);

               Set_NZ (CPU, Size, Y);

               CPU.Write
                 (Dst, Size, Trace_Code,
                  CPU.Image.all, Y);

               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Y));
               end if;

            end;
         when Triple_Set_Instruction =>
            declare
               Size         : constant Data_Size := Get_Size (Op);
               Src_1, Src_2 : Operand_Type;
               Dst          : Operand_Type;
               X, Y         : Word;
               R            : Boolean;
            begin
               Src_1 := Next_Operand (CPU);

               CPU.Read
                 (Src_1, Size, Trace_Code,
                  CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               CPU.Read
                 (Src_2, Size, Trace_Code,
                  CPU.Image.all, Y);

               Dst := Next_Operand (CPU);

               case Triple_Set_Instruction (Instruction) is
                  when A_Seq_3 =>
                     if Trace_Code then
                        Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (X)
                                         & " " & Aqua.IO.Hex_Image (Y));
                     end if;
                     R := X = Y;
                  when A_Sne_3 =>
                     R := X /= Y;
                  when A_Sgt_3 =>
                     R := X > Y;
                  when A_Slt_3 =>
                     R := X < Y;
                  when A_Sge_3 =>
                     R := X >= Y;
                  when A_Sle_3 =>
                     R := X <= Y;
               end case;

               Y := Boolean'Pos (R);

               Set_NZ (CPU, Size, Y);

               CPU.Write
                 (Dst, Size, Trace_Code,
                  CPU.Image.all, Y);

               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Y));
               end if;

            end;

         when Single_Operand_Float_Instruction =>
            null;
         when Double_Operand_Float_Instruction =>
            null;
         when Branch_Instruction =>
            declare
               Condition : constant Condition_Code :=
                             Branch_Info_Array (Instruction).Condition;
               Negated   : constant Boolean :=
                             not Branch_Info_Array (Instruction).Asserted;
               Offset    : constant Word := Next_Value (CPU, Word_16_Size);
            begin
               Handle_Branch (CPU, Condition, Negated, Offset);
            end;

         when A_Jmp | A_Jsr =>
            declare
               Destination : constant Word := Next_Value (CPU, Word_32_Size);
               New_PC      : constant Address :=
                               CPU.Globals (R_PC) + Destination - 4;
            begin
               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Destination));
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (New_PC));
               end if;
               if Instruction = A_Jsr then
                  CPU.R_Jump := CPU.Globals (R_PC);
               end if;
               CPU.Globals (R_PC) := New_PC;
            end;

         when A_Goto =>
            declare
               X : Word;
               Dst : constant Operand_Type :=
                       Next_Operand (CPU);
            begin
               CPU.Read
                 (Operand => Dst,
                  Size    => Word_16_Size,
                  Trace   => Trace_Code,
                  Memory  => CPU.Image.all,
                  Value   => X);
               CPU.Globals (R_PC) := X;
            end;

         when A_Trap =>

            Handle_Trap
              (CPU, Natural (Op and 2#0000_1111#));

      end case;

      if CPU.Globals (R_PC) < 16#1000# then
         raise Runtime_Error with
           "PC = " & Aqua.IO.Hex_Image (CPU.Globals (R_PC));
      end if;

      if Trace_Calls
        and then (Instruction = A_Call or else Instruction = A_Return)
      then
         Ada.Text_IO.New_Line;
         CPU.Show_Stack;
      elsif Trace_Code then
         Ada.Text_IO.New_Line;
      end if;

   end Handle;

   ----------------
   -- Handle_Add --
   ----------------

   procedure Handle_Add
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Src + Dst);
   end Handle_Add;

   ----------------
   -- Handle_And --
   ----------------

   procedure Handle_And
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Src and Dst);
   end Handle_And;

   -------------------
   -- Handle_Branch --
   -------------------

   procedure Handle_Branch
     (CPU         : in out Aqua_CPU_Type'Class;
      Condition   : Condition_Code;
      Negate      : Boolean;
      Offset      : Word)
   is
      Branch : Boolean;
   begin
      case Condition is
         when Always =>
            Branch := True;
         when EQ =>
            Branch := CPU.Z;
         when LT =>
            Branch := CPU.N or else CPU.V;
         when LE =>
            Branch := not (CPU.N xor CPU.V);
         when MI =>
            Branch := CPU.N;
         when LOS =>
            Branch := CPU.C or else CPU.Z;
         when VS =>
            Branch := CPU.V;
         when CS =>
            Branch := CPU.C;
      end case;
      if Negate then
         Branch := not Branch;
      end if;

      if Branch then
         if Offset = 0 then
            null;
         elsif Offset < 16#8000# then
            CPU.Globals (R_PC) := CPU.Globals (R_PC) + Offset;
         else
            CPU.Globals (R_PC) := CPU.Globals (R_PC) - (16#1_0000# - Offset);
         end if;

         if Trace_Code then
            Ada.Text_IO.Put_Line
              ("branch: "
               & Aqua.IO.Hex_Image (CPU.Globals (R_PC)));
         end if;

      end if;

   end Handle_Branch;

   ----------------
   -- Handle_Clr --
   ----------------

   procedure Handle_Clr
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, 0);
   end Handle_Clr;

   ----------------
   -- Handle_Cmp --
   ----------------

   procedure Handle_Cmp
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
   begin
      Set_NZ (CPU, Size, Dst - Src);
   end Handle_Cmp;

   ----------------
   -- Handle_Dec --
   ----------------

   procedure Handle_Dec
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
      X : Word := Get (Dst, Size);
   begin
      X := X - 1;
      Set (Dst, Size, X);
   end Handle_Dec;

   ----------------
   -- Handle_Div --
   ----------------

   procedure Handle_Div
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst / Src);
   end Handle_Div;

   ----------------
   -- Handle_Inc --
   ----------------

   procedure Handle_Inc
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
      X : Word := Get (Dst, Size);
   begin
      X := X + 1;
      Set (Dst, Size, X);
   end Handle_Inc;

   ----------------
   -- Handle_Ldj --
   ----------------

   procedure Handle_Ldj
     (CPU      : in out Aqua_CPU_Type'Class;
      Size     : Aqua.Data_Size;
      Dst      : in out Aqua.Word)
   is
      pragma Unreferenced (Size);
   begin
      CPU.R_Jump := Dst;
   end Handle_Ldj;

   ----------------
   -- Handle_Mov --
   ----------------

   procedure Handle_Mov
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      Data : constant Word := Get (Src, Size);
   begin
      if Trace_Code then
         Ada.Text_IO.Put (" " & CPU.Show (Data));
      end if;
      Set (Dst, Size, Get (Src, Size));
   end Handle_Mov;

   ----------------
   -- Handle_Mul --
   ----------------

   procedure Handle_Mul
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst * Src);
   end Handle_Mul;

   ---------------
   -- Handle_Or --
   ---------------

   procedure Handle_Or
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst or Src);
   end Handle_Or;

   ----------------
   -- Handle_Stj --
   ----------------

   procedure Handle_Stj
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (Size);
   begin
      Dst := CPU.R_Jump;
   end Handle_Stj;

   ----------------
   -- Handle_Sub --
   ----------------

   procedure Handle_Sub
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst - Src);
   end Handle_Sub;

   -----------------
   -- Handle_Trap --
   -----------------

   procedure Handle_Trap
     (CPU   : in out Aqua_CPU_Type'Class;
      Index : Natural)
   is
   begin

      if Trace_Code then
         Ada.Text_IO.Put_Line ("trap" & Natural'Image (Index));
      end if;

      case Index is
         when Aqua.Traps.Get_Data_Segment_Start =>
            CPU.Set_R (0, CPU.Image.Segment_Base ("heap"));

         when Aqua.Traps.End_Execution =>
            declare
               Exit_Code : constant Word := CPU.Pop;
            begin
               if Exit_Code /= 0 then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "exited with code" & Exit_Code'Img);
               end if;
               CPU.B := True;
            end;

--           when Aqua.Traps.Handle_Exception =>
--
--              loop
--                 declare
--                    Trap_Word : constant Word := CPU.Pop;
--                    Trap_Addr : constant Address := Get_Address (Trap_Word);
--                    Handler_Addr : constant Address :=
--                                  CPU.Image.Get_Handler_Address (Trap_Addr);
--                 begin
--
--                    if Trap_Addr = 0 then
--                       raise Aqua.Execution.Execution_Error with
--                         "unhandled exception at "
--                         & Aqua.IO.Hex_Image (Trap_Addr)
--                         & ": "
--                         & CPU.Show (CPU.R (0));
--
--                    elsif Handler_Addr = 0 then
--                       CPU.R (R_SP) := CPU.R (R_FP);
--                       CPU.R (R_FP) := CPU.Pop;
--
--                    else
--                       CPU.R (Architecture.R_PC) :=
--                         To_Address_Word (Handler_Addr);
--                       exit;
--                    end if;
--                 end;
--              end loop;

         when others =>
            raise Constraint_Error
              with "unimplemented trap:" & Natural'Image (Index);
      end case;
   end Handle_Trap;

   ----------------
   -- Handle_Xor --
   ----------------

   procedure Handle_Xor
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst xor Src);
   end Handle_Xor;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type)
   is null;

   ------------------
   -- Next_Operand --
   ------------------

   function Next_Operand
     (CPU : in out Aqua_CPU_Type'Class)
      return Aqua.Architecture.Operand_Type
   is
      X : constant Octet := Next_Octet (CPU);
      Op : constant Natural := Natural (X / 32);
   begin
      if Trace_Code then
         Ada.Text_IO.Put
           (" " & Aqua.IO.Hex_Image (X));
      end if;
      CPU.Operand_Acc (Op) := CPU.Operand_Acc (Op) + 1;
      return Operand : constant Aqua.Architecture.Operand_Type :=
        Get_Operand (X)
      do
         null;
      end return;
   end Next_Operand;

   ----------------
   -- Next_Value --
   ----------------

   overriding function Next_Value
     (CPU : in out Aqua_CPU_Type;
      Size : Data_Size)
      return Word
   is
      PC     : Word renames CPU.Globals (R_PC);
      Result : Word := CPU.Image.Get_Word (PC);
   begin
      case Size is
         when Word_8_Size =>
            Result := Result mod 256;
            PC := PC + 1;
         when Word_16_Size =>
            Result := Result mod 65536;
            PC := PC + 2;
         when Word_32_Size =>
            PC := PC + 4;
      end case;

      return Result;
   end Next_Value;

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

--        Put_Line
--          ("Memory:"
--           & " reserved ="
--           & Address'Image (CPU.Image.Code_Low)
--           & " code ="
--           & Address'Image (CPU.Image.Code_High - CPU.Image.Code_Low)
--           & " heap ="
--           & Address'Image
--             (CPU.Image.Heap_High - CPU.Image.Code_High)
--           & " stack ="
--           & Address'Image (Default_Stack_Top - CPU.R (R_SP) + 1)
--           & " allocated ="
--           & Natural'Image (CPU.Image.Used_Memory));

      Put_Line ("CPU time:"
                & Natural'Image (Natural (CPU.Exec_Time * 1000.0))
                & "ms");
   end Report;

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

   ------------
   -- Set_NZ --
   ------------

   procedure Set_NZ
     (CPU   : in out Aqua_CPU_Type'Class;
      Size  : Data_Size;
      Value : Word)
   is
      X : constant Word := Get (Value, Size);
   begin
      CPU.Z := X = 0;

      case Size is
         when Word_8_Size =>
            CPU.N := X mod 256 >= 16#80#;
         when Word_16_Size =>
            CPU.N := X mod 65536 >= 16#8000#;
         when Word_32_Size =>
            CPU.N := X >= 16#8000_0000#;
      end case;
   end Set_NZ;

   -----------
   -- Set_R --
   -----------

   overriding procedure Set_R
     (CPU : in out Aqua_CPU_Type;
      R   : Aqua.Architecture.Register_Index;
      X   : Word)
   is
   begin
      if R >= CPU.R_Global then
         CPU.Globals (R) := X;
      else
         if R >= CPU.R_Local then
            for I in CPU.R_Local .. R loop
               CPU.Window (CPU.Window_Index (I)) := 0;
            end loop;
            CPU.R_Local := R + 1;
         end if;
         CPU.Window (CPU.Window_Index (R)) := X;
      end if;
   end Set_R;

   ----------
   -- Show --
   ----------

   overriding function Show
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is (CPU.Image.Show (Value));

   --------------------
   -- Show_Registers --
   --------------------

   procedure Show_Registers
     (CPU : in out Aqua_CPU_Type)
   is
      procedure Put (Name  : String;
                     Value : String);

      ---------
      -- Put --
      ---------

      procedure Put (Name  : String;
                     Value : String)
      is
      begin
         Ada.Text_IO.Put (Name);
         Ada.Text_IO.Set_Col (8);
         Ada.Text_IO.Put (Ada.Strings.Fixed.Trim (Value, Ada.Strings.Both));
         Ada.Text_IO.New_Line;
      end Put;

   begin
      Put ("Zero", Register_Window_Index'Image (CPU.Zero));
      Put ("Local", Register_Index'Image (CPU.R_Local));
      Put ("Global", Register_Index'Image (CPU.R_Global));
      Put ("Jump", Aqua.IO.Hex_Image (CPU.R_Jump));

      for R in 0 .. CPU.R_Local - 1 loop
         Put (Register_Name (R), Aqua.IO.Hex_Image (CPU.Get_R (R)));
      end loop;

      for R in CPU.R_Global .. Register_Index'Last loop
         Put (Register_Name (R), Aqua.IO.Hex_Image (CPU.Get_R (R)));
      end loop;
   end Show_Registers;

   ----------------
   -- Show_Stack --
   ----------------

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      for I in 0 .. CPU.Window_Local - 1 loop
         Ada.Text_IO.Put (Aqua.IO.Hex_Image (CPU.Window (I)));
         Ada.Text_IO.Put (" ");
         if I = CPU.Zero - 1 then
            Ada.Text_IO.Put ("| ");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Show_Stack;

   ---------------------
   -- Update_Register --
   ---------------------

--     procedure Update_Register
--       (CPU     : in out Aqua_CPU_Type'Class;
--        Operand : Aqua.Architecture.Operand_Type)
--     is
--     begin
--        if Operand.Mode /= Small_Immediate
--          and then Operand.Register >= CPU.R_Local
--          and then Operand.Register < CPU.R_Global
--        then
--           if Trace_Code then
--              Ada.Text_IO.Put
--                ("[local"
--                 & Integer'Image (-(Integer (Operand.Register)))
--                 & "]");
--           end if;
--           for R in CPU.R_Local .. Operand.Register loop
--              CPU.R (R) := 0;
--           end loop;
--           CPU.R_Local := Operand.Register + 1;
--        end if;
--     end Update_Register;

end Aqua.CPU;
