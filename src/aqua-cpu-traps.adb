with Ada.Text_IO;

with Aqua.IO;
with Aqua.Iterators;
with Aqua.Objects;
with Aqua.Primitives;
with Aqua.Values;
with Aqua.Words;

package body Aqua.CPU.Traps is

   Trace_Properties : constant Boolean := False;

   -------------------------
   -- Handle_Get_Property --
   -------------------------

   procedure Handle_Get_Property
     (CPU            : in out Aqua_CPU_Type'Class;
      Argument_Count : Natural;
      Property_Name  : Word)
   is
      use Aqua.Architecture;
      Arguments      : Array_Of_Words (1 .. Argument_Count);
      Target         : Word;
      Value          : Aqua.Values.Property_Value;
   begin

      for I in 1 .. Argument_Count loop
         Arguments (I) := CPU.Pop;
      end loop;

      Target := CPU.R (R_OP);

      if not Is_String_Reference (Property_Name) then
         raise Constraint_Error
           with "property name must be a string: "
           & Aqua.IO.Hex_Image (Property_Name);
      end if;

      if Is_String_Reference (Target) then
         declare
            Prim : constant Primitive_Reference :=
                     Aqua.Primitives.Get_Primitive
                       ("string__" & CPU.Image.To_String (Property_Name));
         begin
            if Prim = 0 then
               raise Constraint_Error
                 with "property "
                 & CPU.Show (Property_Name)
                 & " not defined for string "
                 & CPU.Show (Target);
            end if;

            Value :=
              Aqua.Values.To_Word_Value
                (Aqua.Primitives.Call_Primitive
                   (CPU, Prim, Target & Arguments (1 .. Argument_Count)));
            if Trace_Properties then
               Ada.Text_IO.Put_Line
                 (CPU.Show (Target) & "." & CPU.Show (Property_Name) & " -> "
                  & CPU.Show (Aqua.Values.To_Word (Value)));
            end if;
         end;
      elsif Is_Integer (Target) then
         declare
            Prim : constant Primitive_Reference :=
                     Aqua.Primitives.Get_Primitive
                       ("integer__" & CPU.Image.To_String (Property_Name));
         begin
            if Prim = 0 then
               raise Constraint_Error
                 with "property "
                 & CPU.Show (Property_Name)
                 & " not defined for integer values";
            end if;

            Value :=
              Aqua.Values.To_Word_Value
                (Aqua.Primitives.Call_Primitive
                   (CPU, Prim, Target & Arguments (1 .. Argument_Count)));
            if Trace_Properties then
               Ada.Text_IO.Put_Line
                 (CPU.Show (Target) & "." & CPU.Show (Property_Name) & " -> "
                  & CPU.Show (Aqua.Values.To_Word (Value)));
            end if;
         end;
      elsif not Is_External_Reference (Target) then
         if Target = 0 then
            raise Runtime_Error
              with "cannot get property "
              & CPU.Show (Property_Name)
              & " from null or undefined object";
         else
            raise Runtime_Error
              with "property "
              & CPU.Show (Property_Name)
              & " defined only on objects; found "
              & CPU.Show (Target);
         end if;
      else
         declare
            Ext : constant access External_Object_Interface'Class :=
                    CPU.To_External_Object (Target);
         begin
            if Ext.all not in Aqua.Objects.Object_Interface'Class then
               raise Constraint_Error
                 with "property "
                 & CPU.Show (Property_Name)
                 & " defined only on objects; found "
                 & "[" & Aqua.IO.Hex_Image (Target) & "] "
                 & CPU.Show (Target);
            end if;
         end;

         declare
            use Aqua.Objects;
            Target_Object : constant access Object_Interface'Class :=
                              Object_Interface'Class
                                (CPU.To_External_Object
                                   (Target).all)'Access;
            Name : constant String :=
                              CPU.Image.To_String (Property_Name);
         begin
            Value := Target_Object.Get_Property (Name);
            if Trace_Properties then
               Ada.Text_IO.Put_Line
                 (Target_Object.Name & "." & CPU.Show (Property_Name) & " -> "
                  & CPU.Show (Aqua.Values.To_Word (Value)));
            end if;
         end;

         declare
            Word_Value : Word := CPU.To_Word (Value);
         begin
            if Aqua.Words.Is_Primitive_Reference (Word_Value) then
               Word_Value :=
                 Aqua.Primitives.Call_Primitive
                   (CPU, Aqua.Words.Get_Primitive_Reference (Word_Value),
                    Target & Arguments (1 .. Argument_Count));
               Value := Aqua.Values.To_Word_Value (Word_Value);
            end if;
         end;

      end if;

      CPU.R (R_PV) := CPU.To_Word (Value);

   end Handle_Get_Property;

   --------------------------
   -- Handle_Iterator_Next --
   --------------------------

   procedure Handle_Iterator_Next
     (CPU   : in out Aqua_CPU_Type'Class;
      R     : Aqua.Architecture.Register_Index)
   is
      use Aqua.Architecture;
      use Aqua.Iterators;
      Iterator_Word : constant Word := CPU.R (R_CTR);
      Iterator_Ext  : access External_Object_Interface'Class;
      Iterator      : access Aqua_Iterator_Interface'Class;
   begin
      if not Is_External_Reference (Iterator_Word) then
         raise Constraint_Error
           with "iterator_next: expected an object but found "
           & CPU.Show (Iterator_Word);
      end if;

      Iterator_Ext := CPU.To_External_Object (Iterator_Word);

      if Iterator_Ext.all not in Aqua_Iterator_Interface'Class then
         raise Constraint_Error
           with "iterator_next: cannot iterate over "
           & Iterator_Ext.Name;
      end if;

      Iterator :=
        Aqua_Iterator_Interface'Class (Iterator_Ext.all)'Access;

      if Trace_Properties then
         Ada.Text_IO.Put_Line
           ("Iterator_Next: old current = "
            & CPU.Show (Iterator.Current));
      end if;

      Iterator.Next (CPU.Z);
      if not CPU.Z then
         if Trace_Properties then
            Ada.Text_IO.Put_Line
              ("Iterator_Next: new current = "
               & " [" & Aqua.IO.Hex_Image (Iterator.Current) & "] "
               & CPU.Show (Iterator.Current));
         end if;
         CPU.R (R) := Iterator.Current;
      else
         if Trace_Properties then
            Ada.Text_IO.Put_Line
              ("Iterator_Next: finished");
         end if;
      end if;
   end Handle_Iterator_Next;

   ---------------------------
   -- Handle_Iterator_Start --
   ---------------------------

   procedure Handle_Iterator_Start
     (CPU   : in out Aqua_CPU_Type'Class)
   is
      use Aqua.Architecture;
      use Aqua.Iterators;
      Container_Word : constant Word := CPU.R (R_CTR);
      Container_Ext  : access External_Object_Interface'Class;
      Container      : access Aqua_Container_Interface'Class;
   begin
      if not Is_External_Reference (Container_Word) then
         raise Constraint_Error
           with "iterator_start: expected an object but found "
           & CPU.Show (Container_Word);
      end if;

      Container_Ext := CPU.To_External_Object (Container_Word);

      if Container_Ext.all not in Aqua_Container_Interface'Class then
         raise Constraint_Error
           with "iterator_start: cannot iterate over "
           & Container_Ext.Name;
      end if;

      Container :=
        Aqua_Container_Interface'Class (Container_Ext.all)'Access;

      if Trace_Properties then
         Ada.Text_IO.Put_Line
           ("iterating: " & CPU.Show (Container_Word));
      end if;

      declare
         It : constant External_Object_Access :=
                new Aqua_Iterator_Interface'Class'
                  (Container.Start);
      begin
         CPU.R (R_CTR) := CPU.To_Word (It);
      end;
   end Handle_Iterator_Start;

   -------------------------
   -- Handle_Set_Property --
   -------------------------

   procedure Handle_Set_Property
     (CPU  : in out Aqua_CPU_Type'Class;
      Name : Word)
   is
      Target : constant Word := CPU.R (Aqua.Architecture.R_OP);
      Value  : constant Word := CPU.R (Aqua.Architecture.R_PV);
   begin
      if Trace_Properties then
         Ada.Text_IO.Put_Line
           ("set: "
            & CPU.Name (Target)
            & "."
            & CPU.Show (Name)
            & " <- "
            & CPU.Show (Value));
      end if;

      if not Is_String_Reference (Name) then
         raise Constraint_Error
           with "set: expected a string for property name, but found "
           & CPU.Show (Name);
      end if;

      if not Is_External_Reference (Target) then
         raise Constraint_Error
           with "set: " & CPU.Show (Name) & ": expected an object but found "
           & CPU.Show (Target);
      end if;

      --                 pragma Assert (Is_String_Reference (Name));
      --                 pragma Assert (Is_External_Reference (Target));

      declare
         Ext : constant access External_Object_Interface'Class :=
                 CPU.To_External_Object (Target);
      begin
         if Ext.all not in Aqua.Objects.Object_Interface'Class then
            raise Constraint_Error
              with "property "
              & CPU.Show (Name)
              & " defined only on objects; found "
              & Ext.Name;
         end if;
      end;

      declare
         use Aqua.Objects;
         Target_Object : constant access Object_Interface'Class :=
                           Object_Interface'Class
                             (CPU.To_External_Object
                                (Target).all)'Access;
         Property_Name : constant String :=
                           CPU.Image.To_String (Name);
      begin
         Target_Object.Set_Property (Property_Name, Value);
      end;
   end Handle_Set_Property;

end Aqua.CPU.Traps;
