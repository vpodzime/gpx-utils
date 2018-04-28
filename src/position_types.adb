package body Position_Types is
   use Ada.Strings.Unbounded;

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

   function "&" (Pre : Unbounded_String; C : Coord) return Unbounded_String is
      use Ada.Strings.Unbounded;
      use type Unbounded_String;
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

   function "&" (Pre : Unbounded_String; E : Elevation_Type)
                return Unbounded_String is
      use Ada.Strings.Unbounded;
      use type Unbounded_String;
      Img : String := Elevation_Type'Image(E);
   begin
      --  remove the leading space if any (instead of the '-' char)
      if Img'Length > 0 and then Img(Img'First) = ' ' then
         return Pre & Img(2..Img'Length);
      else
         return Pre & Img;
      end if;
   end;

   function "&" (Pre : String; Point : Position) return String is
      ("" & Point.Lat & ", " & Point.Lon & " (" & Point.Elevation & ")");

   function "&" (Pre : Unbounded_String; Point : Position)
                return Unbounded_String is
      (To_Unbounded_String("") & Point.Lat & ", " & Point.Lon & " (" & Point.Elevation & ")");
end Position_Types;
