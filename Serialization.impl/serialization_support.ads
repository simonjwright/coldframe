--  $Id: serialization_support.ads,v fbe698e22b63 2003/01/24 06:25:15 simon $

with BC.Support.Memory_Streams;
with ColdFrame.Serialization;

package Serialization_Support is

   Maximum_Record_Size : constant := 1024;

   subtype Base is ColdFrame.Serialization.Base;

   subtype Base_Class is Base'Class;

   subtype Buffer
      is BC.Support.Memory_Streams.Stream_Type (Maximum_Record_Size);

end Serialization_Support;
