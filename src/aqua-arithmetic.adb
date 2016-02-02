package body Aqua.Arithmetic is

   ---------
   -- Dec --
   ---------

   procedure Dec
     (X : in out Word;
      Y : in     Positive := 1)
   is
   begin
      if Is_Address (X) then
         declare
            T : Address := Get_Address (X);
         begin
            T := T - Address (Y);
            X := To_Address_Word (T);
         end;
      elsif Is_Integer (X) then
         declare
            T : Aqua_Integer := Get_Integer (X);
         begin
            T := T - Aqua_Integer (Y);
            X := To_Integer_Word (T);
         end;
      else
         X := X - Word (Y);
      end if;
   end Dec;

   ---------
   -- Inc --
   ---------

   procedure Inc
     (X : in out Word;
      Y : in     Positive := 1)
   is
   begin
      if Is_Address (X) then
         declare
            T : Address := Get_Address (X);
         begin
            T := T + Address (Y);
            X := To_Address_Word (T);
         end;
      elsif Is_Integer (X) then
         declare
            T : Aqua_Integer := Get_Integer (X);
         begin
            T := T + Aqua_Integer (Y);
            X := To_Integer_Word (T);
         end;
      else
         X := X + Word (Y);
      end if;
   end Inc;

   ----------------------
   -- Relative_Address --
   ----------------------

   function Relative_Address
     (Location : Address;
      Reference : Address)
      return Address
   is
      Result : constant Address := Reference - Location;
   begin
      return Result;
   end Relative_Address;

end Aqua.Arithmetic;
