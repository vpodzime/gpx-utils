with Sax.Readers;
with Unicode.CES;
with Sax.Attributes; use Sax.Attributes;

package body GPX_Reader is

   overriding procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is

      Pos: Position;
   begin
      if Local_Name = "trkpt" then
         Pos.Lon := Coord'Value (Get_Value (Atts, "lon"));
         Pos.Lat := Coord'Value (Get_Value (Atts, "lat"));
         Handler.Points.Append (Pos);
      end if;
   end Start_Element;

   overriding procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is null;
   --  begin
   --     if Local_Name = "trkpt" then
   --        Handler.N_Points := Handler.N_Points + 1;
   --     end if;
   --  end End_Element;

end GPX_Reader;
