--  $Id: serialization_support.ads,v 7b585abe1423 2003/01/22 22:36:20 simon $

with Ada.Unchecked_Deallocation;
with ColdFrame.Serialization;

package Serialization_Support is

   subtype Base is ColdFrame.Serialization.Base;

   subtype Base_Class is Base'Class;

   type Base_Class_P is access Base_Class;

   procedure Free is new Ada.Unchecked_Deallocation
     (Base_Class, Base_Class_P);

end Serialization_Support;
