with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Code is
   type Matrix_Type is array (Natural range <>, Natural range <>) of Integer;

   Min_Distance : Integer := -1;
   Times        : Natural := 0;
   Distance     : Integer := 0;

   procedure Print_Matrix (M : Matrix_Type) is
   begin
      for I in M'Range (1) loop
         for J in M'Range (2) loop
            Put (M (I, J)'Image & " ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   ---------
   -- Min --
   ---------

   function Min (A, B, C : Integer) return Integer is
      Min : Integer := A;
   begin
      if B < Min then
         Min := B;
      end if;

      if C < Min then
         Min := C;
      end if;

      return Min;
   end Min;

   -----------------
   -- Levenshtein --
   -----------------

   function Levenshtein (Str1 : in String; Str2 : in String) return Integer is
      M : Integer := Str1'Length;
      N : Integer := Str1'Length;

      Matrix : Matrix_Type (0 .. M + 1, 0 .. N + 1) :=
        (others => (others => 0));

      Cost : Integer := 0;
   begin
      for I in Matrix'Range (1) loop
         Matrix (I, 0) := I;
      end loop;

      Print_Matrix (Matrix);

      for J in Matrix'Range (2) loop
         Matrix (0, J) := J;
      end loop;

      Print_Matrix (Matrix);

      for I in 1 .. M loop
         for J in 1 .. M loop

            if Str1 (I) = Str2 (J) then
               Cost := 0;
            else
               Cost := 1;
            end if;

            Matrix (I, J) :=
              Min
                (Matrix (I - 1, J) + 1, Matrix (I, J - 1) + 1,
                 Matrix (I - 1, J - 1) + Cost);
         end loop;
      end loop;

      Print_Matrix (Matrix);

      return Matrix (M, N);
   end Levenshtein;

begin
   for I in 1 .. Argument_Count loop
      for J in 1 .. Argument_Count loop

         if I /= J then
            Distance := Levenshtein (Argument (I), Argument (J));

            if (Min_Distance = -1) or (Min_Distance > Distance) then
               Min_Distance := Distance;
            end if;
            Times := Times + 1;
         end if;

      end loop;
   end loop;

   Put_Line ("times: " & Times'Image);
   Put_Line ("min_distance: " & Min_Distance'Image);

end Code;
