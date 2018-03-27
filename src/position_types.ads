with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Position_Types is
   type Coord is delta 0.000001 digits 8;

   function "&" (Pre : String; C : Coord) return String;
   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; C : Coord) return Ada.Strings.Unbounded.Unbounded_String;

   type Position is record
      Lon: Coord;
      Lat: Coord;
   end record;

   package Position_Vector is new Ada.Containers.Vectors(Natural, Position);
end Position_Types;
