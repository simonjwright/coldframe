with ColdFrame.Project.Serialization;

package Serial_Test_Support is

   subtype Base is ColdFrame.Project.Serialization.Base;

   type T is new Base with record
      I : Integer;
      F : Float;
      C : Character;
   end record;

end Serial_Test_Support;
