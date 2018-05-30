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
      Lon: Coord := 0.00;
      Lat: Coord := 0.00;
      Elevation: Elevation_Type := 0.00;
   end record;

   Unknown_Elevation : constant Elevation_Type := 0.00;

   function "&" (Pre : String; Point : Position) return String;
   function "&" (Pre : Ada.Strings.Unbounded.Unbounded_String; Point : Position) return Ada.Strings.Unbounded.Unbounded_String;

   package Position_Vector is new Ada.Containers.Vectors(Natural, Position);
end Position_Types;
