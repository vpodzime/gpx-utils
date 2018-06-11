with Ada.Containers.Vectors;

with Position_Types;

generic
   type Point is new Position_Types.Position with private;
   with package Point_Vector is new Ada.Containers.Vectors(Natural, Point);
package Elevator is
   --  Coord_Len : constant := 9;          --  e.g. "17.156381"
   --  type Coord_Str is new String
   --    with Dynamic_Predicate => Coord_Str'Length = Coord_Len;

   procedure Elevate_Points (Points : in out Point_Vector.Vector; API_Key: in String);
end Elevator;
