with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Maps;

with Aqua.Architecture;
with Aqua.Drivers.Meta_Driver;
with Aqua.IO;
with Aqua.Options;

package body Aqua.CPU is

   Trace_Code        : Boolean := False;
   Trace_Executions  : constant Boolean := False;
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

   ---------------
   -- Add_Watch --
   ---------------

   procedure Add_Watch
     (CPU             : in out Aqua_CPU_Type'Class;
      Watched_Address : Address)
   is null;
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
      Profile : constant Boolean := Aqua.Options.Profile;
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

         if Profile then
            declare
               Loc : constant String :=
                       CPU.Image.Show_Source_Position
                         (CPU.PC);
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
      null;
   end Initialize;

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
