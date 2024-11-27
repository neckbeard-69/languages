with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Code is

   function Fibbonaci (N : Integer) return Integer is
   begin
      if N = 0 then
         return 0;
      end if;

      if N = 1 then
         return 1;
      end if;

      return Fibbonaci (N - 1) + Fibbonaci (N - 2);
   end Fibbonaci;

   U : constant Integer := Integer'Value (Argument (1));
   R : Integer := 0;

begin
   for I in 1 .. U loop
      R := R + Fibbonaci (I);
   end loop;

   Put_Line (R'Image);
end Code;
