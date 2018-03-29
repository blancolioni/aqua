with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;

with Ada.Text_IO;

with Aqua.Arithmetic;
with Aqua.CPU.Traps;
with Aqua.Debug;
with Aqua.IO;
with Aqua.Objects;
with Aqua.Primitives;
with Aqua.Traps;

package body Aqua.CPU is

   use Aqua.Architecture;

   Trace_Properties : constant Boolean := False;
   Trace_Code       : Boolean := False;
   Trace_Stack      : constant Boolean := False;
   Trace_Executions : constant Boolean := False;

   Current_Output : Ada.Text_IO.File_Type;

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
     (CPU : in out Aqua_CPU_Type'Class;
      Size : Data_Size)
      return Word;

   function Next_Octet
     (CPU : in out Aqua_CPU_Type'Class)
      return Octet
   is (Octet (Next_Value (CPU, Word_8_Size)));

   function Next_Operand
     (CPU : in out Aqua_CPU_Type'Class)
      return Aqua.Architecture.Operand_Type
   is (Get_Operand (Next_Octet (CPU)));

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
      use Aqua.Arithmetic;
      PC : Word renames CPU.R (Aqua.Architecture.R_PC);
      SP : Word renames CPU.R (Aqua.Architecture.R_SP);
      FP : Word renames CPU.R (Aqua.Architecture.R_FP);
   begin

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

      PC := To_Address_Word (Start);
      CPU.B := False;

      while not CPU.B
        and then PC /= 0
      loop
         declare
            Op : constant Octet :=
                   CPU.Image.Get_Octet (Get_Address (PC));
            Original_PC : constant Word := PC;
         begin
            if Trace_Code then
               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Get_Address (FP))
                  & " "
                  & Aqua.IO.Hex_Image (Get_Address (SP))
                  & " "
                  & Aqua.IO.Hex_Image (Get_Address (PC))
                  & ": ");

               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Op)
                  & " "
                  & Aqua.Debug.Opcode_Image (Op));

            end if;
            Inc (PC);

            begin
               Handle (CPU, Op);
               if Trace_Code then
                  Ada.Text_IO.New_Line;
               end if;
            exception
               when E : Runtime_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     CPU.Image.Show_Source_Position
                       (Get_Address (Original_PC))
                     & ": error: "
                     & Ada.Exceptions.Exception_Message (E));

                  CPU.Dump_Core;
                  while PC /= 0 loop
                     SP := FP;
                     FP := CPU.Pop;
                     PC := CPU.Pop;
                     if PC /= 0 then
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "  at "
                           & CPU.Image.Show_Source_Position
                             (Get_Address (PC)));
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
         Aqua.Arithmetic.Inc (SP, 4 * Arguments'Length);
      end if;

      PC := CPU.Pop;

      CPU.Exec_Time := CPU.Exec_Time + Ada.Calendar.Clock - CPU.Start;

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
            if Is_String_Reference (CPU.R (0)) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Message: " & CPU.To_String (CPU.R (0)));
            end if;
            CPU.B := True;

         when A_Nop =>
            null;
         when A_Rts =>
            PC := CPU.Pop;
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
                    (Dst, Size, Trace_Code, CPU.R, CPU.Image.all, X);
               else
                  if Dst.Mode = Register and then not Dst.Deferred then
                     A := 0;
                     X := Aqua.Get (CPU.R (Dst.Register), Size);
                  elsif Dst.Mode = Literal then
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

               if Instruction = A_Tst then
                  null;
               elsif Dst.Mode = Register and then not Dst.Deferred then
                  Aqua.Set (CPU.R (Dst.Register), Size, X);
               elsif Dst.Mode = Literal then
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
                 (Src, Size, Trace_Code, CPU.R, CPU.Image.all, X);

               Dst := Next_Operand (CPU);

               if Instruction = A_Cmp then
                  Aqua.Architecture.Read
                    (Dst, Size, Trace_Code, CPU.R, CPU.Image.all, Y);
               else
                  if Dst.Mode = Register and then not Dst.Deferred then
                     A := 0;
                     Y := Aqua.Get (CPU.R (Dst.Register), Size);
                  elsif Dst.Mode = Literal then
                     A := 0;
                     Y := Word (Dst.Lit);
                  else
                     A := Get_Address (Dst, Size, Trace_Code,
                                       CPU.R, CPU.Image.all);
                     Y := CPU.Image.Get_Value (A, Size);
                  end if;
               end if;

               Double_Operand (Instruction) (CPU, Size, X, Y);

               Set_NZ (CPU, Size, Y);

               if Instruction = A_Cmp then
                  null;
               elsif Dst.Mode = Register and then not Dst.Deferred then
                  Aqua.Set (CPU.R (Dst.Register), Size, Y);
               elsif Dst.Mode = Literal then
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
                 (Src_1, Size, Trace_Code, CPU.R, CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               Aqua.Architecture.Read
                 (Src_2, Size, Trace_Code, CPU.R, CPU.Image.all, Y);

               Dst := Next_Operand (CPU);

               Double_Operand (Convert_Triple_To_Double (Instruction))
                 (CPU, Size, X, Y);

               Set_NZ (CPU, Size, Y);

               Aqua.Architecture.Write
                 (Dst, Size, Trace_Code, CPU.R, CPU.Image.all, Y);
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
                 (Src_1, Size, Trace_Code, CPU.R, CPU.Image.all, X);

               Src_2 := Next_Operand (CPU);

               Aqua.Architecture.Read
                 (Src_2, Size, Trace_Code, CPU.R, CPU.Image.all, Y);

               Dst := Next_Operand (CPU);

               case Triple_Set_Instruction (Instruction) is
                  when A_Seq_3 | A_Sne_3 =>
                     if Size = Word_32_Size
                       and then Is_String_Reference (X)
                       and then Is_String_Reference (Y)
                     then
                        R := X = Y
                          or else (CPU.To_String (X) = CPU.To_String (Y));
                     else
                        R := X = Y;
                     end if;

                     if Instruction = A_Sne_3 then
                        R := not R;
                     end if;
                  when A_Sgt_3 =>
                     R := Get_Integer (X) > Get_Integer (Y);
                  when A_Slt_3 =>
                     R := Get_Integer (X) < Get_Integer (Y);
                  when A_Sge_3 =>
                     R := Get_Integer (X) >= Get_Integer (Y);
                  when A_Sle_3 =>
                     R := Get_Integer (X) <= Get_Integer (Y);
               end case;

               Y := Boolean'Pos (R);

               Set_NZ (CPU, Size, Y);

               Aqua.Architecture.Write
                 (Dst, Size, Trace_Code, CPU.R, CPU.Image.all, Y);
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
                               Get_Address (PC)
                               + Get_Address (Destination)
                               - 4;
            begin
               if Instruction = A_Jsr then
                  CPU.Push (PC);
               end if;
               PC := To_Address_Word (New_PC);
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

         when A_Get_Property =>
            declare
               Argument_Count : constant Natural :=
                                  Natural (Op mod 16);
               Name_Word      : constant Word :=
                                  Next_Value (CPU, Word_32_Size);
            begin
               if Trace_Code then
                  Ada.Text_IO.Put
                    (" " & CPU.Show (Name_Word));
               end if;
               Aqua.CPU.Traps.Handle_Get_Property
                 (CPU, Argument_Count, Name_Word);
               Set_NZ (CPU, Word_32_Size, CPU.R (Aqua.Architecture.R_PV));
            end;

         when A_Set_Property =>
            declare
               Name_Word      : constant Word :=
                                  Next_Value (CPU, Word_32_Size);
            begin
               if Trace_Code then
                  Ada.Text_IO.Put
                    (" " & CPU.Show (Name_Word));
               end if;
               Aqua.CPU.Traps.Handle_Set_Property
                 (CPU, Name_Word);
            end;

         when A_Allocate =>
            declare
               Base_Word : constant Word :=
                             CPU.R (R_OP);
               New_Word  : Word;
            begin
               if Is_External_Reference (Base_Word) then
                  declare
                     Ext : constant access External_Object_Interface'Class :=
                             CPU.To_External_Object (Base_Word);
                     New_Value : constant External_Object_Access :=
                                   new External_Object_Interface'Class'
                                     (Ext.all);
                  begin
                     New_Value.Set_Reference (0);
                     New_Word := CPU.To_Word (New_Value);
                  end;
               else
                  New_Word := Base_Word;
               end if;

               CPU.R (R_OP) := New_Word;
            end;

         when A_Trap =>

            Handle_Trap
              (CPU, Natural (Op and 2#0000_1111#));

         when A_Iterator_Start =>
            Traps.Handle_Iterator_Start (CPU);
         when A_Iterator_Next =>
            declare
               R : constant Register_Index := Register_Index (Op mod 16);
            begin
               Traps.Handle_Iterator_Next (CPU, R);
            end;
      end case;

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
   begin
      if Is_String_Reference (Src) or else Is_String_Reference (Dst) then
         declare
            Left : constant String :=
                     (if Is_String_Reference (Src) or else Is_Integer (Src)
                      then CPU.To_String (Src)
                      else CPU.Show (Src));
            Right : constant String :=
                      (if Is_String_Reference (Dst) or else Is_Integer (Dst)
                       then CPU.To_String (Dst)
                       else CPU.Show (Dst));
         begin
            Dst := CPU.To_String_Word (Left & Right);
         end;
      else
         declare
            R : constant Word :=
                  (Src and Payload_Mask) + (Dst and Payload_Mask);
         begin
            Aqua.Set
              (Dst, Size,
               (R and Payload_Mask) or (Dst and not Payload_Mask));
         end;
      end if;
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
      R : constant Word :=
            (Src and Payload_Mask) and (Dst and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
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
            Aqua.Arithmetic.Inc
              (CPU.R (R_PC), Integer (Offset));
         else
            Aqua.Arithmetic.Dec
              (CPU.R (R_PC),
               Integer ((16#1_0000# - Offset)));
         end if;

         if Trace_Code then
            Ada.Text_IO.Put_Line
              ("branch: "
               & Aqua.IO.Hex_Image (CPU.R (R_PC)));
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
      if Size = Word_32_Size
        and then Is_Integer (Src) and then Is_Integer (Dst)
      then
         Dst := To_Integer_Word
           (Get_Integer (Src) - Get_Integer (Dst));
      elsif Size = Word_32_Size
        and then Is_String_Reference (Src)
        and then Is_String_Reference (Dst)
      then
         if Src = Dst
           or else (CPU.To_String (Src) = CPU.To_String (Dst))
         then
            Dst := 0;
         else
            Dst := 1;
         end if;
      else
         if Get (Src, Size) = Get (Dst, Size) then
            Dst := 0;
         else
            Dst := 1;
         end if;
      end if;

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
      Aqua.Arithmetic.Dec (X);
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
      R : constant Word :=
            (Dst and Payload_Mask) / (Src and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
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
      Aqua.Arithmetic.Inc (X);
      Set (Dst, Size, X);
   end Handle_Inc;

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
      R : constant Word :=
            (Src and Payload_Mask) * (Dst and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
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
      R : constant Word :=
            (Src and Payload_Mask) or (Dst and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
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
      R : constant Word :=
            (Dst and Payload_Mask) - (Src and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
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
         when Aqua.Traps.Allocate =>
            declare
               New_Item : constant External_Object_Access :=
                            new Aqua.Objects.Root_Object_Type;
            begin
               CPU.Ext.Append (New_Item);
               CPU.Push
                 (To_External_Word
                    (External_Reference
                         (CPU.Ext.Last_Index)));
            end;
         when Aqua.Traps.Join_Strings =>
            declare
               Right_Word : constant Word := CPU.Pop;
               Left_Word  : constant Word := CPU.Pop;
               pragma Assert (Is_Integer (Right_Word)
                              or else Is_String_Reference (Right_Word));
               pragma Assert (Is_Integer (Right_Word)
                              or else Is_String_Reference (Left_Word));
               Result     : constant String :=
                              CPU.To_String (Left_Word)
                            & CPU.To_String (Right_Word);
            begin
               CPU.Push
                 (CPU.To_String_Word (Result));
            end;

         when Aqua.Traps.IO_Put_String =>
            declare
               W : constant Word := CPU.Pop;
               S : constant String := CPU.To_String (W);
            begin
               Ada.Text_IO.Put (S);
            end;

         when Aqua.Traps.IO_Put_Char =>
            declare
               W : constant Word := CPU.Pop;
               Ch : constant Character :=
                      Character'Val (Get_Integer (W));
            begin
               if Ch = Character'Val (10) then
                  Ada.Text_IO.New_Line;
               else
                  Ada.Text_IO.Put (Ch);
               end if;
            end;

         when Aqua.Traps.String_Substitute =>
            declare
               Replace_With    : constant Word := CPU.Pop;
               Replace_String  : constant Word := CPU.Pop;
               Base_Text       : constant Word := CPU.Pop;
            begin

               if Trace_Properties then
                  Ada.Text_IO.Put_Line
                    ("sub: "
                     & CPU.Show (Base_Text)
                     & ": ["
                     & CPU.Show (Replace_String)
                     & "] -> ["
                     & CPU.Show (Replace_With)
                     & "]");
               end if;

               pragma Assert (Is_String_Reference (Base_Text));
               pragma Assert (Is_String_Reference (Replace_String));
               pragma Assert (Is_String_Reference (Replace_With));

               declare
                  use Ada.Strings.Fixed;
                  use Ada.Strings.Unbounded;
                  Result : Unbounded_String;
                  S : constant String := CPU.To_String (Base_Text);
                  X : constant String := CPU.To_String (Replace_String);
                  Y : constant String := CPU.To_String (Replace_With);
                  Index : Positive := S'First;
               begin
                  while Index <= S'Length - X'Length loop
                     if S (Index .. Index + X'Length - 1) = X then
                        Result := Result & Y;
                        Index := Index + X'Length;
                     else
                        Result := Result & S (Index);
                        Index := Index + 1;
                     end if;
                  end loop;

                  Result := Result & S (Index .. S'Last);

                  CPU.Push (CPU.To_String_Word (To_String (Result)));
               end;
            end;

         when Aqua.Traps.IO_Set_Output =>
            declare
               Name_Value : constant Word := CPU.Pop;
            begin
               if Name_Value = 0
                 or else (Is_String_Reference (Name_Value)
                          and then CPU.To_String (Name_Value) = "")
               then
                  Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
                  Ada.Text_IO.Close (Current_Output);
               elsif not Is_String_Reference (Name_Value) then
                  raise Constraint_Error
                    with "Set_Output: expected a string,but found "
                    & CPU.Show (Name_Value);
               else
                  Ada.Text_IO.Create (Current_Output,
                                      Ada.Text_IO.Out_File,
                                      CPU.To_String (Name_Value));
                  Ada.Text_IO.Set_Output (Current_Output);
               end if;
            end;

         when Aqua.Traps.Report_State =>
            CPU.Report;

         when Aqua.Traps.Handle_Exception =>

            loop
               declare
                  Trap_Word : constant Word := CPU.Pop;
                  Trap_Addr : constant Address := Get_Address (Trap_Word);
                  Handler_Addr : constant Address :=
                                   CPU.Image.Get_Handler_Address (Trap_Addr);
               begin

                  if Trap_Addr = 0 then
                     raise Aqua.Execution.Execution_Error with
                       "unhandled exception at "
                       & Aqua.IO.Hex_Image (Trap_Addr)
                       & ": "
                       & CPU.Show (CPU.R (0));

                  elsif Handler_Addr = 0 then
                     CPU.R (R_SP) := CPU.R (R_FP);
                     CPU.R (R_FP) := CPU.Pop;

                  else
                     CPU.R (Architecture.R_PC) :=
                       To_Address_Word (Handler_Addr);
                     exit;
                  end if;
               end;
            end loop;

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
      R : constant Word :=
            (Src and Payload_Mask) xor (Dst and Payload_Mask);
   begin
      Aqua.Set
        (Dst, Size,
         (R and Payload_Mask) or (Dst and not Payload_Mask));
   end Handle_Xor;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (CPU : in out Aqua_CPU_Type)
   is
   begin
      Aqua.Primitives.Load_Primitive_Objects (CPU);
   end Initialize;

   ----------
   -- Name --
   ----------

   function Name
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
   begin
      if Is_Address (Value) then
         return Aqua.IO.Hex_Image (Get_Address (Value));
      elsif Is_Integer (Value) then
         return "#" & Aqua_Integer'Image (Get_Integer (Value));
      elsif Is_External_Reference (Value) then
         return CPU.To_External_Object (Value).Name;
      elsif Is_String_Reference (Value) then
         return CPU.To_String (Value);
      else
         return Aqua.IO.Hex_Image (Value);
      end if;
   end Name;

   ----------------
   -- Next_Value --
   ----------------

   function Next_Value
     (CPU : in out Aqua_CPU_Type'Class;
      Size : Data_Size)
      return Word
   is
   begin
      return Result : constant Word :=
        CPU.Image.Get_Value (Get_Address (CPU.R (R_PC)), Size)
      do
         CPU.R (R_PC) := CPU.R (R_PC) + Data_Octets (Size);
         if Trace_Code then
            Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Result, Size));
         end if;
      end return;
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
              (Get_Address (CPU.R (R_SP)));
   begin
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("pop("
            & Aqua.IO.Hex_Image
              (Get_Address (CPU.R (R_SP)))
            & ")->"
            & Aqua.IO.Hex_Image (X));
      end if;

      Aqua.Arithmetic.Inc
        (CPU.R (R_SP), 4);

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
      Aqua.Arithmetic.Dec (CPU.R (R_SP), 4);
      CPU.Image.Set_Word (Get_Address (CPU.R (R_SP)), Value);
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("push("
            & Aqua.IO.Hex_Image
              (Get_Address (CPU.R (R_SP)))
            & ","
            & Aqua.IO.Hex_Image (Value)
            & ")");
      end if;
   end Push;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (CPU : Aqua_CPU_Type)
   is
      use Ada.Calendar;
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      Put_Line
        ("Memory:"
         & " reserved ="
         & Address'Image (CPU.Image.Code_Low)
         & " code ="
         & Address'Image (CPU.Image.Code_High - CPU.Image.Code_Low)
         & " heap ="
         & Address'Image
           (CPU.Image.Heap_High - CPU.Image.Code_High)
         & " stack ="
         & Address'Image (Address'Last - Get_Address (CPU.R (R_SP)) + 1)
         & " allocated ="
         & Natural'Image (CPU.Image.Used_Memory));

      Put_Line ("Objects:" & Natural'Image (CPU.Ext.Last_Index + 1)
                & "/"
                & Trim
                  (External_Reference'Image (External_Reference'Last),
                   Left));
      Put_Line ("Strings:"
                & Natural'Image (CPU.Str.Last_Index + 1)
                & "/"
                & Trim
                  (String_Reference'Image (String_Reference'Last),
                   Left));
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
      CPU.Z := X = 0
        or else (Size = Word_32_Size
                 and then Is_Address (Value)
                 and then Get_Address (Value) = 0);

      case Size is
         when Word_8_Size =>
            CPU.N := X >= 16#80#;
         when Word_16_Size =>
            CPU.N := X >= 16#8000#;
         when Word_32_Size =>
            CPU.N := (X and Payload_Mask) >= 16#0080_0000#;
      end case;
   end Set_NZ;

   ----------------
   -- Set_Option --
   ----------------

   procedure Set_Option
     (Name  : String;
      Value : String)
   is
   begin
      if Name = "trace-code" then
         Trace_Code := Boolean'Value (Value);
      end if;
   end Set_Option;

   ----------
   -- Show --
   ----------

   overriding function Show
     (CPU : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
      package Word_Maps is
        new Ada.Containers.Ordered_Maps
          (Word, Boolean);

      Shown_Map : Word_Maps.Map;

      function Recursive_Show (X : Word) return String;

      function Internal_Show (X : Word) return String;

      -------------------
      -- Internal_Show --
      -------------------

      function Internal_Show (X : Word) return String is
      begin
         if Shown_Map.Contains (X) then
            return "[cycle]";
         else
            return Recursive_Show (X);
         end if;
      end Internal_Show;

      --------------------
      -- Recursive_Show --
      --------------------

      function Recursive_Show (X : Word) return String is
      begin
         if Is_Address (X) then
            return "@" & Aqua.IO.Hex_Image (Get_Address (X))
              & ": " & CPU.Show (CPU.Image.Get_Word (Get_Address (X)));
         elsif Is_Integer (X) then
            return Ada.Strings.Fixed.Trim
              (Aqua_Integer'Image (Get_Integer (X)),
               Ada.Strings.Left);
         elsif Is_External_Reference (X) then
            Shown_Map.Insert (X, True);
            return CPU.To_External_Object (X).Show (Internal_Show'Access);
         elsif Is_String_Reference (X) then
            return CPU.To_String (X);
         else
            return Aqua.IO.Hex_Image (X);
         end if;
      end Recursive_Show;

   begin
      return Recursive_Show (Value);
   exception
      when others =>
         return "[" & Aqua.IO.Hex_Image (Value) & "]";
   end Show;

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
      if Get_Address (CPU.R (R_SP)) < Address'Last / 2 then
         Ada.Text_IO.Put_Line ("stack fault");
         return;
      end if;

      Ada.Text_IO.Put_Line ("---- stack dump");
      for A in Get_Address (CPU.R (R_SP)) .. Address'Last - 1 loop
         if A mod 4 = 0 then
            Ada.Text_IO.Put_Line
              (Aqua.IO.Hex_Image (A)
               & ": "
               & CPU.Show (CPU.Image.Get_Word (A)));
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("---------------");
   end Show_Stack;

   ------------------------
   -- To_External_Object --
   ------------------------

   overriding function To_External_Object
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return access External_Object_Interface'Class
   is
      Reference : constant Natural :=
                    Natural (Value and Payload_Mask);
   begin
      if Reference = 0 then
         return null;
      else
         return CPU.Ext (Positive (Value and Payload_Mask));
      end if;
   end To_External_Object;

   ----------------
   -- To_Integer --
   ----------------

   overriding function To_Integer
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return Aqua_Integer
   is
   begin
      if Is_Integer (Value) then
         return Get_Integer (Value);
      else
         declare
            S : constant String := CPU.To_String (Value);
         begin
            if S = "" then
               return 0;
            else
               return Aqua_Integer'Value (S);
            end if;
         end;
      end if;
   end To_Integer;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (CPU   : in out Aqua_CPU_Type;
      Value : Word)
      return String
   is
   begin
      if Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Aqua_Integer'Image (Get_Integer (Value)),
            Ada.Strings.Left);
      elsif CPU.Image.Have_String (Value) then
         return CPU.Image.To_String (Value);
      else
         declare
            Index : constant Integer :=
                      Natural (Get_String_Reference (Value))
                      - CPU.Image.String_Count;
         begin
            if Index in 0 .. CPU.Str.Last_Index then
               return CPU.Str (Index);
            else
               return "ERROR: bad string reference:"
                 & Integer'Image (Index);
            end if;
         end;
      end if;
   end To_String;

   --------------------
   -- To_String_Word --
   --------------------

   overriding function To_String_Word
     (CPU  : in out Aqua_CPU_Type;
      Text : String)
      return Word
   is
   begin
      if not CPU.Str_Map.Contains (Text) then
         declare
            Ref : constant String_Reference :=
                    String_Reference
                      (CPU.Image.String_Count
                       + CPU.Str.Last_Index
                       + 1);
         begin
            CPU.Str_Map.Insert
              (Text, To_String_Word (Ref));
            CPU.Str.Append (Text);
         end;
      end if;
      return CPU.Str_Map (Text);
   end To_String_Word;

   -------------
   -- To_Word --
   -------------

   overriding function To_Word
     (CPU  : in out Aqua_CPU_Type;
      Item : not null access External_Object_Interface'Class)
      return Word
   is
      Ref : External_Reference := Item.Get_Reference;
   begin

      if Ref = 0 then

         if CPU.Ext.Last_Index >= Positive (External_Reference'Last) then
            raise Storage_Error
              with "no free objects";
         end if;

         CPU.Ext.Append (External_Object_Access (Item));
         Ref := External_Reference (CPU.Ext.Last_Index);

         Item.Set_Reference (Ref);

      end if;

      return Set_Tag (Word (Ref), External_Tag);
   end To_Word;

end Aqua.CPU;
