with Sax.Readers;
with Unicode.CES;
with Sax.Attributes;
with Position_Types;  use Position_Types;

package GPX_Reader is
   type Reader is tagged record
      Points : Position_Vector.Vector;
   end record;

   procedure Read_Points (R: in out Reader; Path: in String);

   --  type Reader is new Sax.Readers.Reader with private;
   --  type Reader is new Sax.Readers.Reader with record
   --     Points   : Position_Vector.Vector;
   --  end record;

   function Get_N_Points (R: Reader) return Natural;

   --  overriding procedure Start_Element
   --    (Handler       : in out Reader;
   --     Namespace_URI : Unicode.CES.Byte_Sequence := "";
   --     Local_Name    : Unicode.CES.Byte_Sequence := "";
   --     Qname         : Unicode.CES.Byte_Sequence := "";
   --     Atts          : Sax.Attributes.Attributes'Class);

   --  overriding procedure End_Element
   --    (Handler : in out Reader;
   --     Namespace_URI : Unicode.CES.Byte_Sequence := "";
   --     Local_Name    : Unicode.CES.Byte_Sequence := "";
   --     Qname         : Unicode.CES.Byte_Sequence := "");

private
   function Get_N_Points (R: Reader) return Natural is (Natural(R.Points.Length));

end GPX_Reader;
