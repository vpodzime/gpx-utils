with Position_Types;  use Position_Types;

package Elevator is
   --  Coord_Len : constant := 9;          --  e.g. "17.156381"
   --  type Coord_Str is new String
   --    with Dynamic_Predicate => Coord_Str'Length = Coord_Len;

   procedure Elevate_Points (Points : in out Position_Vector.Vector);
end Elevator;
