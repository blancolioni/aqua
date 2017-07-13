with Ada.Containers.Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;

with Aqua.IO;
with Aqua.Objects.Arrays;

package body Aqua.Primitives is

   type Primitive_Object_Info is
      record
         Name     : access String;
         Instance : Primitive_Object_Access;
      end record;

   package Primitive_Object_Vectors is
     new Ada.Containers.Vectors (Positive, Primitive_Object_Info);

   type Function_Handler_Interface is
     new Handler_Interface with
      record
         Handler : Primitive_Handler;
      end record;

   overriding function Handle
     (Primitive : Function_Handler_Interface;
      Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Array_Of_Words)
      return Word
   is (Primitive.Handler (Context, Arguments));

   type Primitive_Function_Info is
      record
         Name           : access String;
         Arg_Count      : Natural;
         Arg_Categories : access Argument_Category;
         Handler        : access Handler_Interface'Class;
      end record;

   package Primitive_Function_Vectors is
     new Ada.Containers.Vectors (Positive, Primitive_Function_Info);

   Prim_Objects   : Primitive_Object_Vectors.Vector;
   Prim_Functions : Primitive_Function_Vectors.Vector;

   --------------------
   -- Call_Primitive --
   --------------------

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Primitive_Reference;
      Arguments : Array_Of_Words)
      return Word
   is
      Info : Primitive_Function_Info
      renames Prim_Functions (Positive (Primitive));
      Actuals : Array_Of_Words (1 .. Info.Arg_Count) :=
                  (others => 0);
      Current : Natural := 0;

      procedure Check
        (Index    : Positive;
         Value    : Word;
         Expected : String;
         OK       : Boolean);

      -----------
      -- Check --
      -----------

      procedure Check
        (Index    : Positive;
         Value    : Word;
         Expected : String;
         OK       : Boolean)
      is
      begin
         if not OK then
            raise Aqua.Execution.Execution_Error
            with "In call to " & Info.Name.all
              & " at argument"
              & Index'Img
              & ": expected " & Expected
              & " but found " & Context.Show (Value)
              & " [" & Aqua.IO.Hex_Image (Value) & "]";
         end if;
      end Check;

   begin
      for Arg of Arguments loop
         Current := Current + 1;
         if Current > Actuals'Last then
            raise Aqua.Execution.Execution_Error
            with "In call to " & Info.Name.all
              & ": too many arguments at "
              & Context.Show (Arg)
              & " [" & Aqua.IO.Hex_Image (Arg) & "]";
         else
            Actuals (Current) := Arg;
            case Info.Arg_Categories (Current) is
               when Any =>
                  null;
               when Object_Argument =>
                  Check (Current, Arg, "object", Is_External_Reference (Arg));
               when Map_Argument =>
                  Check (Current, Arg, "map",
                         Is_External_Reference (Arg)
                         and then Context.To_External_Object (Arg).all
                         in Aqua.Objects.Object_Interface'Class);
               when Array_Argument =>
                  Check (Current, Arg, "array",
                         Is_External_Reference (Arg)
                         and then Context.To_External_Object (Arg).all
                         in Aqua.Objects.Arrays.Root_Array_Type'Class);
               when String_Argument =>
                  Check (Current, Arg, "string",
                         Is_String_Reference (Arg));
               when Integer_Argument =>
                  Check (Current, Arg, "integer",
                         Is_Integer (Arg));
            end case;
         end if;
      end loop;

      return Info.Handler.Handle (Context, Arguments);
   end Call_Primitive;

   --------------------
   -- Call_Primitive --
   --------------------

   function Call_Primitive
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Primitive : Primitive_Reference)
      return Word
   is
      Info : Primitive_Function_Info renames
               Prim_Functions (Positive (Primitive));
      Args : Array_Of_Words (1 .. Info.Arg_Count);
   begin
      for I in Args'Range loop
         Args (I) := Context.Pop;
      end loop;
      return Info.Handler.Handle (Context, Args);
   end Call_Primitive;

   -------------------
   -- Get_Primitive --
   -------------------

   function Get_Primitive
     (Name : String)
      return Primitive_Reference
   is
   begin
      for I in 1 .. Prim_Functions.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive
           (Prim_Functions (I).Name.all, Name)
         then
            return Primitive_Reference (I);
         end if;
      end loop;
      return 0;
   end Get_Primitive;

   ----------------------------
   -- Load_Primitive_Objects --
   ----------------------------

   procedure Load_Primitive_Objects
     (Executor : in out Aqua.Execution.Execution_Interface'Class)
   is
   begin
      for I in 1 .. Prim_Objects.Last_Index loop
         declare
            W : constant Word :=
                  Executor.To_Word (Prim_Objects (I).Instance);
         begin
            if False then
               Ada.Text_IO.Put_Line
                 (Prim_Objects (I).Name.all & ": "
                    & Aqua.IO.Hex_Image (W)
                    & " - " & Executor.Show (W));
            end if;
         end;
      end loop;
   end Load_Primitive_Objects;

   ----------------------------
   -- New_Primitive_Function --
   ----------------------------

   procedure New_Primitive_Function
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Primitive_Handler)
   is
      Arg_Categories : constant Argument_Category (1 .. Argument_Count) :=
                         (others => Any);
   begin
      New_Primitive_Function (Name, Arg_Categories, Handler);
   end New_Primitive_Function;

   ----------------------------
   -- New_Primitive_Function --
   ----------------------------

   procedure New_Primitive_Function
     (Name      : String;
      Arguments : Argument_Category;
      Handler   : Primitive_Handler)
   is
   begin
      Prim_Functions.Append
        ((new String'(Name), Arguments'Length,
         new Argument_Category'(Arguments),
         new Function_Handler_Interface'(Handler => Handler)));
   end New_Primitive_Function;

   ---------------------------
   -- New_Primitive_Handler --
   ---------------------------

   procedure New_Primitive_Handler
     (Name           : String;
      Argument_Count : Natural;
      Handler        : Handler_Interface'Class)
   is
      Arg_Categories : constant Argument_Category (1 .. Argument_Count) :=
                         (others => Any);
   begin
      New_Primitive_Handler (Name, Arg_Categories, Handler);
   end New_Primitive_Handler;

   ---------------------------
   -- New_Primitive_Handler --
   ---------------------------

   procedure New_Primitive_Handler
     (Name           : String;
      Arguments      : Argument_Category;
      Handler        : Handler_Interface'Class)
   is
   begin
      Prim_Functions.Append
        ((new String'(Name), Arguments'Length,
         new Argument_Category'(Arguments),
         new Handler_Interface'Class'(Handler)));
   end New_Primitive_Handler;

   --------------------------
   -- New_Primitive_Object --
   --------------------------

   procedure New_Primitive_Object
     (Name : String;
      Item : not null access External_Object_Interface'Class)
   is
   begin
      Prim_Objects.Append
        ((new String'(Name), Primitive_Object_Access (Item)));
   end New_Primitive_Object;

end Aqua.Primitives;
