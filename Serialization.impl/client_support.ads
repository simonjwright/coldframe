-- $Id: client_support.ads,v 7b585abe1423 2003/01/22 22:36:20 simon $

with ColdFrame.Project.Serialization;

package Client_Support is

   type Rec is new ColdFrame.Project.Serialization.Base with record
      X : Integer;
      Y : Integer;
   end record;

end Client_Support;
