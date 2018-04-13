with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Position_Types is
   type Coord is delta 0.000001 digits 8;
   type Elevation_Type is delta 0.000001 digits 10; --  -9999..9999

   function "&" (Pre : String; C : Coord) return String;
   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; C : Coord) return Ada.Strings.Unbounded.Unbounded_String;

   function "&" (Pre : String; E : Elevation_Type) return String;
   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; E : Elevation_Type)
                return Ada.Strings.Unbounded.Unbounded_String;

   type Position is record
      Lon: Coord;
      Lat: Coord;
      Elevation: Elevation_Type;
   end record;

   package Position_Vector is new Ada.Containers.Vectors(Natural, Position);
end Position_Types;
