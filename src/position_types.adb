package body Position_Types is
   function "&" (Pre : String; C : Coord) return String is
      Img : String := Coord'Image(C);
   begin
      return Pre & Img(2..Img'Length);
   end;

   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; C : Coord) return Ada.Strings.Unbounded.Unbounded_String is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Img : String := Coord'Image(C);
   begin
      return Pre & Img(2..Img'Length);
   end;
end Position_Types;
