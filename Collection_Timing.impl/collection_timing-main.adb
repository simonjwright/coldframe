with Ada.Text_IO; use Ada.Text_IO;
with BC.Support.High_Resolution_Time;
with ColdFrame.Instances;

with Collection_Timing.A;
with Collection_Timing.B;
with Collection_Timing.B_B.Abstract_Containers;
with Collection_Timing.B_B.Collections;
with Collection_Timing.B_U.Abstract_Containers;
with Collection_Timing.B_U.Collections;
with Collection_Timing.Initialize;
with Collection_Timing.Support;
with Collection_Timing.Tear_Down;
with Collection_Timing.U;

procedure Collection_Timing.Main is

   The_A : A.Handle;
   The_Bs : B_B.Collections.Collection;
   The_Us : B_U.Collections.Collection;

   function Clock return BC.Support.High_Resolution_Time.Time
     renames BC.Support.High_Resolution_Time.Clock;
   Start, Finish : BC.Support.High_Resolution_Time.Time;
   use type BC.Support.High_Resolution_Time.Time;

   subtype CIH is ColdFrame.Instances.Handle;

begin

   Collection_Timing.Initialize;

   The_A := A.Create;

   for I in 0 .. E'Last / 2 loop
      B.Link (Owns => B_B.Create ((Id => I * 2)),
              Belongs_To => The_A);
      U.Link (Owns => B_U.Create,
              Belongs_To => The_A);
   end loop;

   Start := Clock;
   The_Bs := B.Belongs_To (The_A);
   Finish := Clock;
   Put_Line ("navigating to the bounded collection: "
               & Duration'Image (Finish - Start));

   Start := Clock;
   The_Us := U.Belongs_To (The_A);
   Finish := Clock;
   Put_Line ("navigating to the unbounded collection: "
               & Duration'Image (Finish - Start));

   declare
      Bs : B_B.Collections.Collection;
   begin
      Start := Clock;
      Bs := The_Bs;
      Finish := Clock;
      Put_Line ("copying the bounded collection: "
                  & Duration'Image (Finish - Start));
   end;

   declare
      Us : B_U.Collections.Collection;
   begin
      Start := Clock;
      Us := The_Us;
      Finish := Clock;
      Put_Line ("copying the unbounded collection: "
                  & Duration'Image (Finish - Start));
   end;

   declare
      It : B_B.Abstract_Containers.Iterator'Class
        :=  B_B.Collections.New_Iterator (The_Bs);
   begin
      while not B_B.Abstract_Containers.Is_Done (It) loop
         Support.CICB.Append (Support.Bounded,
                              CIH (B_B.Abstract_Containers.Current_Item (It)));
         B_B.Abstract_Containers.Next (It);
      end loop;
   end;

   Start := Clock;
   declare
      It : ColdFrame.Instances.Abstract_Containers.Iterator'Class
        := Support.CICB.New_Iterator (Support.Bounded);
      Result : B_B.Collections.Collection;
   begin
      while not ColdFrame.Instances.Abstract_Containers.Is_Done (It) loop
         B_B.Collections.Append
           (Result,
            B_B.Handle
              (ColdFrame.Instances.Abstract_Containers.Current_Item (It)));
         ColdFrame.Instances.Abstract_Containers.Next (It);
      end loop;
      Finish := Clock;
      Put_Line ("converting the bounded collection: "
                  & Duration'Image (Finish - Start));
   end;

   declare
      It : B_U.Abstract_Containers.Iterator'Class
        :=  B_U.Collections.New_Iterator (The_Us);
   begin
      while not B_U.Abstract_Containers.Is_Done (It) loop
         Support.CICU.Append (Support.Unbounded,
                              CIH (B_U.Abstract_Containers.Current_Item (It)));
         B_U.Abstract_Containers.Next (It);
      end loop;
   end;

   Start := Clock;
   declare
      It : ColdFrame.Instances.Abstract_Containers.Iterator'Class
        := Support.CICU.New_Iterator (Support.Unbounded);
      Result : B_U.Collections.Collection;
   begin
      while not ColdFrame.Instances.Abstract_Containers.Is_Done (It) loop
         B_U.Collections.Append
           (Result,
            B_U.Handle
              (ColdFrame.Instances.Abstract_Containers.Current_Item (It)));
         ColdFrame.Instances.Abstract_Containers.Next (It);
      end loop;
      Finish := Clock;
      Put_Line ("converting the unbounded collection: "
                  & Duration'Image (Finish - Start));
   end;

   Collection_Timing.Tear_Down;

end Collection_Timing.Main;
