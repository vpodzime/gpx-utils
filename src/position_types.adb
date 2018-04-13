package body Position_Types is
   function "&" (Pre : String; C : Coord) return String is
      Img : String := Coord'Image(C);
   begin
      --  remove the leading space if any (instead of the '-' char)
      if Img'Length > 0 and then Img(Img'First) = ' ' then
         return Pre & Img(2..Img'Length);
      else
         return Pre & Img;
      end if;
   end;

   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; C : Coord) return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use type Ada.Strings.Unbounded.Unbounded_String;
      Img : String := Coord'Image(C);
   begin
      --  remove the leading space if any (instead of the '-' char)
      if Img'Length > 0 and then Img(Img'First) = ' ' then
         return Pre & Img(2..Img'Length);
      else
         return Pre & Img;
      end if;
   end;

   function "&" (Pre : String; E : Elevation_Type) return String is
      Img : String := Elevation_Type'Image(E);
   begin
      --  remove the leading space if any (instead of the '-' char)
      if Img'Length > 0 and then Img(Img'First) = ' ' then
         return Pre & Img(2..Img'Length);
      else
         return Pre & Img;
      end if;
   end;

   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; E : Elevation_Type)
                return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      use type Ada.Strings.Unbounded.Unbounded_String;
      Img : String := Elevation_Type'Image(E);
   begin
      --  remove the leading space if any (instead of the '-' char)
      if Img'Length > 0 and then Img(Img'First) = ' ' then
         return Pre & Img(2..Img'Length);
      else
         return Pre & Img;
      end if;
   end;
end Position_Types;
