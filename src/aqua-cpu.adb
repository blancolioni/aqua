with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;

with Ada.Text_IO;

with WL.String_Maps;

with Aqua.Debug;
with Aqua.IO;
with Aqua.Traps;

with Aqua.Options;

with Aqua.Drivers.Meta_Driver;

package body Aqua.CPU is

   use Aqua.Architecture;

   Trace_Code        : Boolean := False;
   Trace_FP          : constant Boolean := True;
   Trace_SP          : constant Boolean := True;
   Trace_Stack       : constant Boolean := False;
   Trace_Executions  : constant Boolean := False;
   Write_Frequencies : constant Boolean := False;

   package Profile_Maps is
     new WL.String_Maps (Natural);

   type Profile_Entry (Length : Positive) is
      record
         Location : String (1 .. Length);
         Hit_Count : Natural;
      end record;

   function ">" (Left, Right : Profile_Entry) return Boolean
   is (Left.Hit_Count > Right.Hit_Count);

   Profile_Map : Profile_Maps.Map;

   package Profile_Hit_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Profile_Entry);

   Source_Location_Width : constant := 24;

   Default_Stack_Top : constant := 16#8000_0000#;

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
     procedure (Size : Aqua.Data_Size;
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

   procedure Handle_Mod
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
     (Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Dec
     (Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   procedure Handle_Inc
     (Size : Aqua.Data_Size;
      Dst  : in out Aqua.Word);

   Double_Operand : constant array (Double_Operand_Instruction)
     of Double_Operand_Handler :=
       (A_Mov  => Handle_Mov'Access,
        A_Cmp  => Handle_Cmp'Access,
        A_Add  => Handle_Add'Access,
        A_Div  => Handle_Div'Access,
        A_Sub  => Handle_Sub'Access,
        A_Mul  => Handle_Mul'Access,
        A_Mod  => Handle_Mod'Access,
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

   function Next_Value
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Data_Size)
      return Word;

   function Next_Octet
     (CPU : in out Aqua_CPU_Type'Class)
      return Octet
   is (Octet (Next_Value (CPU, Word_8_Size)));

   function Next_Operand
     (CPU : in out Aqua_CPU_Type'Class)
      return Aqua.Architecture.Operand_Type;

   procedure Dump_Core
     (CPU : in out Aqua_CPU_Type'Class);

   ---------------
   -- Add_Watch --
   ---------------

   procedure Add_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address)
   is
   begin
      CPU.Image.Add_Monitor (Watched_Address);
   end Add_Watch;

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
         when A_Mod_3 =>
            return A_Mod;
      end case;
   end Convert_Triple_To_Double;

   ----------------
   -- Create_CPU --
   ----------------

   function Create_CPU (Image : Aqua.Images.Image_Type) return Aqua_CPU is
   begin
      return CPU : constant Aqua_CPU := new Aqua_CPU_Type do
         CPU.Image := Image;
         Aqua.Drivers.Meta_Driver.Create_Meta_Driver (CPU.Image);
      end return;
   end Create_CPU;

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
      CPU.Show_Stack;
      CPU.Report;

      Set_Output (Standard_Output);
      Close (File);
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unabled to create core file");
   end Dump_Core;

   ----------------------
   -- Enable_Profiling --
   ----------------------

   procedure Enable_Profiling (Enabled : Boolean) is
   begin
      Aqua.Options.Set_Option ("profile", Enabled'Image);
   end Enable_Profiling;

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
      PC : Word renames CPU.R (Aqua.Architecture.R_PC);
      SP : Word renames CPU.R (Aqua.Architecture.R_SP);
      FP : Word renames CPU.R (Aqua.Architecture.R_FP);
      Last_Source : Ada.Strings.Unbounded.Unbounded_String;

      Profile : constant Boolean := Aqua.Options.Profile;

   begin

      Trace_Code := Aqua.Options.Trace_Code;

      CPU.Opcode_Acc := (others => 0);
      CPU.Operand_Acc := (others => 0);

      CPU.Set_Current_Environment (Environment_Name);

      if Trace_Code or else Trace_Executions then
         Ada.Text_IO.Put_Line
           ("start: ["
            & Environment_Name
            & "] "
            & Aqua.IO.Hex_Image (PC));
      end if;

      CPU.Start := Ada.Calendar.Clock;
      CPU.Push (PC);

      for Arg of reverse Arguments loop
         CPU.Push (Arg);
      end loop;

      CPU.Push (0);

      PC := Start;
      CPU.B := False;

      while not CPU.B
        and then PC /= 0
      loop
         declare
            Op : constant Octet :=
                   CPU.Image.Get_Octet (PC);
            Original_PC : constant Word := PC;
         begin
            CPU.Opcode_Acc (Op) := CPU.Opcode_Acc (Op) + 1;

            if Profile then
               declare
                  Loc : constant String :=
                          CPU.Image.Show_Source_Position
                            (Original_PC);
               begin
                  if not Profile_Map.Contains (Loc) then
                     Profile_Map.Insert (Loc, 0);
                  end if;

                  declare
                     N : Natural renames Profile_Map (Loc);
                  begin
                     N := N + 1;
                  end;
               end;
            end if;

            if Trace_Code then
               declare
                  Loc : constant String :=
                          CPU.Image.Show_Source_Position
                            (Original_PC);
               begin
                  if Last_Source /= Loc then
                     Last_Source := To_Unbounded_String (Loc);
                     if Loc'Length < Source_Location_Width then
                        Ada.Text_IO.Put (Loc);
                     else
                        declare
                           First_Index : constant Positive :=
                                           Loc'Last + 5
                                             - Source_Location_Width;
                        begin
                           Ada.Text_IO.Put
                             ("..." & Loc (First_Index .. Loc'Last));
                        end;
                     end if;
                  end if;
               end;
               Ada.Text_IO.Set_Col
                 (Ada.Text_IO.Count (Source_Location_Width + 1));
               if Trace_FP then
                  Ada.Text_IO.Put
                    (Aqua.IO.Hex_Image (FP) & " ");
               end if;

               if Trace_SP then
                  Ada.Text_IO.Put
                    (Aqua.IO.Hex_Image (SP) & " ");
               end if;

               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (PC)
                  & ": ");

               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Op)
                  & " "
                  & Aqua.Debug.Opcode_Image (Op));

            end if;

            PC := PC + 1;

            begin
               CPU.Image.Begin_Transaction;
               Handle (CPU, Op);
               CPU.Image.End_Transaction;

               if Trace_Code then
                  Ada.Text_IO.New_Line;
               end if;

            exception
               when E : Runtime_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     CPU.Image.Show_Source_Position
                       (Original_PC)
                     & ": error: "
                     & Ada.Exceptions.Exception_Message (E));

                  CPU.Dump_Core;
                  loop
                     SP := FP;
                     FP := CPU.Pop;
                     PC := CPU.Pop;
                     exit when PC = 0;
                     if PC /= 0 then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "  at "
                           & CPU.Image.Show_Source_Position (PC));
                     end if;
                  end loop;

                  CPU.B := True;
                  CPU.Report;
                  Ada.Text_IO.Put_Line ("core dumped");
                  raise;
            end;

         end;
      end loop;

      if Arguments'Length > 0 then
         SP := SP + 4 * Word (Arguments'Length);
      end if;

      PC := CPU.Pop;

      CPU.Exec_Time := CPU.Exec_Time + Ada.Calendar.Clock - CPU.Start;

      if Write_Frequencies then
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

            Freq  : Opcode_Frequency_Tables.List;
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
      end if;
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

   ------------
   -- Handle --
   ------------

   procedure Handle
     (CPU : in out Aqua_CPU_Type'Class;
      Op  : in     Octet)
   is
      PC : Word renames CPU.R (Aqua.Architecture.R_PC);
      Instruction : constant Aqua_Instruction :=
                      Get_Instruction (Op);
   begin
      case Instruction is
         when A_Halt =>
            CPU.Dump_Core;
            CPU.Report;
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "HALT; core dumped");

            declare
               Addr : Address := CPU.R (0);
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
            PC := CPU.Pop;
            if Trace_Code then
               Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (PC));
            end if;
         when A_Return =>
            declare
               Top : Saved_Registers renames
                       CPU.R_Stack.First_Element;
            begin
               for R in 0 .. Top.Count - 1 loop
                  CPU.R (Register_Index (R)) := Top.Rs (R);
               end loop;
               PC := CPU.Pop;
               CPU.R_Stack.Delete_First;
            end;
         when Single_Operand_Instruction =>
            declare
               Size : constant Data_Size := Get_Size (Op);
               Dst  : Operand_Type;
               A    : Address;
               X    : Word;
            begin
               Dst := Next_Operand (CPU);

               if Instruction = A_Tst then
                  Aqua.Architecture.Read
                    (Dst, Size, Trace_Code,
                     CPU.R_Local, CPU.R_Global,
                     CPU.R, CPU.Image.all, X);
               else
                  if Dst.Mode = Register and then not Dst.Deferred then
                     A := 0;
                     X := Aqua.Get (CPU.R (Dst.Register), Size);
                  elsif Dst.Mode = Small_Immediate then
                     A := 0;
                     X := Word (Dst.Lit);
                  else
                     A := Get_Address (Dst, Size, Trace_Code,
                                       CPU.R, CPU.Image.all);
                     X := CPU.Image.Get_Value (A, Size);
                  end if;
               end if;

               if Instruction /= A_Tst then
                  Single_Operand (Instruction) (Size, X);
               end if;

               Set_NZ (CPU, Size, X);

               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (X));
               end if;

               if Instruction = A_Tst then
                  null;
               elsif Dst.Mode = Register and then not Dst.Deferred then
                  Aqua.Set (CPU.R (Dst.Register), Size, X);
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

               Aqua.Architecture.Read
                 (Src, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, X);

               Dst := Next_Operand (CPU);

               if Dst.Mode /= Small_Immediate
                 and then Dst.Register > CPU.R_Local
                 and then Dst.Register < CPU.R_Global
               then
                  CPU.R (Dst.Register) := 0;
                  CPU.R_Local := Dst.Register;
               end if;

               if Instruction = A_Cmp then
                  Aqua.Architecture.Read
                    (Dst, Size, Trace_Code,
                     CPU.R_Local, CPU.R_Global,
                     CPU.R, CPU.Image.all, Y);
               else
                  if Dst.Mode = Register and then not Dst.Deferred then
                     A := 0;
                     Y := Aqua.Get (CPU.R (Dst.Register), Size);
                  elsif Dst.Mode = Small_Immediate then
                     A := 0;
                     Y := Word (Dst.Lit);
                  else
                     A := Get_Address (Dst, Size, Trace_Code,
                                       CPU.R, CPU.Image.all);
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
               elsif Dst.Mode = Register and then not Dst.Deferred then
                  Aqua.Set (CPU.R (Dst.Register), Size, Y);
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

               Aqua.Architecture.Read
                 (Src_1, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               Aqua.Architecture.Read
                 (Src_2, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, Y);

               Dst := Next_Operand (CPU);

               Double_Operand (Convert_Triple_To_Double (Instruction))
                 (CPU, Size, X, Y);

               Set_NZ (CPU, Size, Y);

               Aqua.Architecture.Write
                 (Dst, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, Y);

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

               Aqua.Architecture.Read
                 (Src_1, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               Aqua.Architecture.Read
                 (Src_2, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, Y);

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

               Aqua.Architecture.Write
                 (Dst, Size, Trace_Code,
                  CPU.R_Local, CPU.R_Global,
                  CPU.R, CPU.Image.all, Y);

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
               New_PC      : constant Address := PC + Destination - 4;
            begin
               if Trace_Code then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Destination));
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (New_PC));
               end if;
               if Instruction = A_Jsr then
                  CPU.Push (PC);
               end if;
               PC := New_PC;
            end;

         when A_Goto =>
            declare
               X : Word;
               Dst : constant Operand_Type :=
                       Next_Operand (CPU);
            begin
               Aqua.Architecture.Read
                 (Operand => Dst,
                  Size    => Word_16_Size,
                  Trace   => Trace_Code,
                  Local   => CPU.R_Local,
                  Global  => CPU.R_Global,
                  R       => CPU.R,
                  Memory  => CPU.Image.all,
                  Value   => X);
               PC := X;
            end;

         when A_Call =>
            declare
               N : constant Saved_Register_Count :=
                     Saved_Register_Count (Op mod 8);
               Saved_Rs : Saved_Registers;
               X : Word;
               Dst : constant Operand_Type :=
                       Next_Operand (CPU);
            begin
               Aqua.Architecture.Read
                 (Operand => Dst,
                  Size    => Word_32_Size,
                  Trace   => Trace_Code,
                  Local   => CPU.R_Local,
                  Global  => CPU.R_Global,
                  R       => CPU.R,
                  Memory  => CPU.Image.all,
                  Value   => X);
               CPU.Push (PC);
               PC := X;

               Saved_Rs.Count := N;
               if N > 0 then
                  for I in 0 .. N - 1 loop
                     Saved_Rs.Rs (I) := CPU.R (Register_Index (I));
                  end loop;
               end if;

               CPU.R_Stack.Insert (CPU.R_Stack.First, Saved_Rs);
            end;

         when A_Trap =>

            Handle_Trap
              (CPU, Natural (Op and 2#0000_1111#));

      end case;

      if PC < 16#1000# then
         raise Runtime_Error with
           "PC = " & Aqua.IO.Hex_Image (PC);
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
      PC     : Word renames CPU.R (R_PC);
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
            PC := PC + Offset;
         else
            PC := PC - (16#1_0000# - Offset);
         end if;

         if Trace_Code then
            Ada.Text_IO.Put (" [taken]");
         end if;

      end if;

   end Handle_Branch;

   ----------------
   -- Handle_Clr --
   ----------------

   procedure Handle_Clr
     (Size     : Aqua.Data_Size;
      Dst      : in out Aqua.Word)
   is
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
     (Size     : Aqua.Data_Size;
      Dst      : in out Aqua.Word)
   is
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
     (Size     : Aqua.Data_Size;
      Dst      : in out Aqua.Word)
   is
      X : Word := Get (Dst, Size);
   begin
      X := X + 1;
      Set (Dst, Size, X);
   end Handle_Inc;

   ----------------
   -- Handle_Mod --
   ----------------

   procedure Handle_Mod
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Aqua.Data_Size;
      Src  : Aqua.Word;
      Dst  : in out Aqua.Word)
   is
      pragma Unreferenced (CPU);
   begin
      Aqua.Set (Dst, Size, Dst mod Src);
   end Handle_Mod;

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
            CPU.R (0) := CPU.Image.Segment_Base ("heap");

         when Aqua.Traps.End_Execution =>
            declare
               Exit_Code : constant Word := CPU.Pop;
            begin
               if Exit_Code /= 0 then
                  declare
                     A : constant Address := Exit_Code;
                     Length : constant Word := CPU.Image.Get_Word (A);
                     Got_Message : Boolean := Length < 200;
                  begin
                     if Got_Message then
                        declare
                           Message : String (1 .. Natural (Length));
                        begin
                           for I in 1 .. Length loop
                              declare
                                 Code : constant Word :=
                                          CPU.Image.Get_Word (A + I * 4);
                              begin
                                 if Code not in 32 .. 127 then
                                    Got_Message := False;
                                    exit;
                                 else
                                    Message (Positive (I)) :=
                                      Character'Val (Code);
                                 end if;
                              end;
                           end loop;

                           if Got_Message then
                              Ada.Text_IO.New_Line;
                              Ada.Text_IO.Put_Line (Message);
                           end if;
                        end;
                     end if;

                     if not Got_Message then
                        Ada.Text_IO.New_Line;
                        Ada.Text_IO.Put_Line
                          ("exit code " & Aqua.IO.Hex_Image (Length));
                     end if;

                     declare
                        PC : Word renames CPU.R (Aqua.Architecture.R_PC);
                        SP : Word renames CPU.R (Aqua.Architecture.R_SP);
                        FP : Word renames CPU.R (Aqua.Architecture.R_FP);
                     begin
                        while PC /= 0 loop
                           SP := FP;
                           FP := CPU.Pop;
                           PC := CPU.Pop;
                           if PC /= 0 then
                              Ada.Text_IO.Put_Line
                                (Ada.Text_IO.Standard_Error,
                                 "  at "
                                 & CPU.Image.Show_Source_Position (PC));
                           end if;
                        end loop;
                     end;
                  end;
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
            raise Runtime_Error
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
   is
   begin
      null;
   end Initialize;

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
      CPU.Operand_Acc (Op) := CPU.Operand_Acc (Op) + 1;
      return Get_Operand (X);
   end Next_Operand;

   ----------------
   -- Next_Value --
   ----------------

   function Next_Value
     (CPU  : in out Aqua_CPU_Type'Class;
      Size : Data_Size)
      return Word
   is
      PC     : Word renames CPU.R (R_PC);
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

      if Trace_Code then
         Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Result, Size));
      end if;
      return Result;
   end Next_Value;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (CPU : in out Aqua_CPU_Type)
      return Word
   is
      X : constant Word :=
            CPU.Image.Get_Word
              (CPU.R (R_SP));
   begin
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("pop("
            & Aqua.IO.Hex_Image
              (CPU.R (R_SP))
            & ")->"
            & Aqua.IO.Hex_Image (X));
      end if;

      CPU.R (R_SP) := CPU.R (R_SP) + 4;

      return X;

   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
   is
   begin
      CPU.R (R_SP) := CPU.R (R_SP) - 4;
      CPU.Image.Set_Word (CPU.R (R_SP), Value);
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("push("
            & Aqua.IO.Hex_Image
              (CPU.R (R_SP))
            & ","
            & Aqua.IO.Hex_Image (Value)
            & ")");
      end if;
   end Push;

   ------------------
   -- Remove_Watch --
   ------------------

   procedure Remove_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address)
   is
   begin
      CPU.Image.Remove_Monitor (Watched_Address);
   end Remove_Watch;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (CPU : Aqua_CPU_Type)
   is
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
   begin
      for R in CPU.R'Range loop
         if CPU.R (R) /= 16#BAAD_F00D# then
            Ada.Text_IO.Put (Register_Name (R));
            Ada.Text_IO.Set_Col (8);
            Ada.Text_IO.Put_Line (CPU.Show (CPU.R (R)));
         end if;
      end loop;
   end Show_Registers;

   ----------------
   -- Show_Stack --
   ----------------

   procedure Show_Stack
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      if CPU.R (R_SP) not in Default_Stack_Top / 2 .. Default_Stack_Top then
         Ada.Text_IO.Put_Line ("stack fault (" & CPU.Show (CPU.R (R_SP))
                               & ")");
         return;
      end if;

      Ada.Text_IO.Put_Line ("---- stack dump");
      for A in CPU.R (R_SP) .. Default_Stack_Top - 1 loop
         if A mod 4 = 0 then
            Ada.Text_IO.Put_Line
              (Aqua.IO.Hex_Image (A)
               & ": "
               & CPU.Show (CPU.Image.Get_Word (A)));
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("---------------");
   end Show_Stack;

   -------------------
   -- Write_Profile --
   -------------------

   procedure Write_Profile (Path : String) is
      use Ada.Text_IO;
      File : File_Type;
      Total : Natural := 0;
      Hit_List : Profile_Hit_Lists.List;

      package Hit_Count_Sorting is
        new Profile_Hit_Lists.Generic_Sorting (">");

   begin

      Put ("writing profile"
           & (if Path = "" then "" else " to " & Path)
           & "..");
      Flush;

      if Path /= "" then
         Create (File, Out_File, Path);
         Set_Output (File);
      end if;

      for Position in Profile_Map.Iterate loop
         declare
            Location  : constant String := Profile_Maps.Key (Position);
            Hit_Count : constant Natural :=
                          Profile_Maps.Element (Position);
         begin
            Hit_List.Append
              ((Location'Length, Location, Hit_Count));
            Total := Total + Hit_Count;
         end;
      end loop;

      Hit_Count_Sorting.Sort (Hit_List);

      for Hit of Hit_List loop
         Put (Hit.Location);
         Set_Col (60);
         Put (Hit.Hit_Count'Image);
         Set_Col (70);
         Put (Natural'Image
              (Natural (Float (Hit.Hit_Count) / Float (Total) * 100.0))
              & "%");
         New_Line;
      end loop;

      if Path /= "" then
         Set_Output (Standard_Output);
         Close (File);
      end if;

      Put_Line (". done");

   exception
      when Name_Error =>
         Put_Line (Standard_Error,
                   Path & ": cannot open for writing");

   end Write_Profile;

end Aqua.CPU;
