with Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded.Text_Io; use Ada.Strings.Unbounded.Text_Io;
with Problem_Reporting.Problem_Report;
with Problem_Reporting.Problem_Report.Collections;
with Problem_Reporting.Problem_Report.All_Instances;
with Problem_Reporting.Problem_Report.Filter_Function;
with Problem_Reporting.Problem_Report.Selection_Function;

procedure T is

  function "+" (Source : String) return Unbounded_String
    renames To_Unbounded_String;

  use type Ada.Calendar.Time;

  Loops : constant := 10_000;

begin

  -- insert some instance
  for I in 1 .. 5 loop
    declare
      This : Problem_Reporting.Problem_Report.Handle;
    begin
      This := Problem_Reporting.Problem_Report.Create ((Id => I));
      Problem_Reporting.Problem_Report.Set_Details (This, +("detail" & I'Img));
      Problem_Reporting.Problem_Report.Set_Reporter (This, +"simon");
    end;
  end loop;

  -- find and report instances (including some that are missing)
  for I in 0 .. 6 loop
    declare
      This : Problem_Reporting.Problem_Report.Handle;
    begin
      This := Problem_Reporting.Problem_Report.Find ((Id => I));
      Put ("for id" & I'Img & ": ");
      Put (" details |");
      Put (Problem_Reporting.Problem_Report.Get_Details (This));
      Put ("|, reporter |");
      Put (Problem_Reporting.Problem_Report.Get_Reporter (This));
      Put_Line ("|");
    exception
      when others =>
        Put_Line (" ** exception for id" & I'Img);
    end;
  end loop;

  -- delete a couple of instances
  Problem_Reporting.Problem_Report.Delete ((Id => 2));
  Problem_Reporting.Problem_Report.Delete ((Id => 3));

  -- find and report instances (including some more that are missing)
  for I in 0 .. 6 loop
    declare
      This : Problem_Reporting.Problem_Report.Handle;
    begin
      This := Problem_Reporting.Problem_Report.Find ((Id => I));
      Put ("for id" & I'Img & ": ");
      Put (" details |");
      Put (Problem_Reporting.Problem_Report.Get_Details (This));
      Put ("|, reporter |");
      Put (Problem_Reporting.Problem_Report.Get_Reporter (This));
      Put_Line ("|");
    exception
      when others =>
        Put_Line (" ** exception for id" & I'Img);
    end;
  end loop;

  -- iterate over all the instances
  declare
    procedure Report (This : Problem_Reporting.Problem_Report.Handle;
                      OK : out Boolean) is
    begin
      Put ("id:"  & Problem_Reporting.Problem_Report.Get_Id (This)'Img);
      Put (" details: ");
      Put (Problem_Reporting.Problem_Report.Get_Details (This));
      Put (", reporter ");
      Put (Problem_Reporting.Problem_Report.Get_Reporter (This));
      New_Line;
      OK := True;
    end Report;
    procedure Visit_All is
      new Problem_Reporting.Problem_Report.Abstract_Containers.Visit (Report);
    C : Problem_Reporting.Problem_Report.Collections.Collection;
    It : Problem_Reporting.Problem_Report.Abstract_Containers.Iterator'Class
       := Problem_Reporting.Problem_Report.Collections.New_Iterator (C);
  begin
    C := Problem_Reporting.Problem_Report.All_Instances;
    Visit_All (It);
  end;

  -- check filtering
  for I in 7 .. 11 loop
    declare
      This : Problem_Reporting.Problem_Report.Handle;
    begin
      This := Problem_Reporting.Problem_Report.Create ((Id => I));
      Problem_Reporting.Problem_Report.Set_Details (This, +("detail" & I'Img));
      Problem_Reporting.Problem_Report.Set_Reporter (This, +"liz");
    end;
  end loop;
  declare
    procedure Report
       (C : Problem_Reporting.Problem_Report.Collections.Collection) is
      procedure Report (This : Problem_Reporting.Problem_Report.Handle;
                        OK : out Boolean) is
      begin
        Put ("id:"  & Problem_Reporting.Problem_Report.Get_Id (This)'Img);
        Put (" details: ");
        Put (Problem_Reporting.Problem_Report.Get_Details (This));
        Put (", reporter ");
        Put (Problem_Reporting.Problem_Report.Get_Reporter (This));
        New_Line;
        OK := True;
      end Report;
      procedure Visit_All is
        new Problem_Reporting.Problem_Report.Abstract_Containers.Visit (Report);
      It : Problem_Reporting.Problem_Report.Abstract_Containers.Iterator'Class
         := Problem_Reporting.Problem_Report.Collections.New_Iterator (C);
    begin
      Visit_All (It);
    end Report;
    function Choose
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean;
    function Filter is new Problem_Reporting.Problem_Report.Filter_Function
       (Pass => Choose);
    function Choose
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean is
    begin
      return Problem_Reporting.Problem_Report.Get_Reporter (This) = +"liz";
    end Choose;
    All_Instances : Problem_Reporting.Problem_Report.Collections.Collection;
    Result : Problem_Reporting.Problem_Report.Collections.Collection;
  begin
    All_Instances := Problem_Reporting.Problem_Report.All_Instances;
    Result := Filter (All_Instances);
    Put_Line ("all instances:");
    Report (All_Instances);
    Put_Line ("just liz:");
    Report (Result);
  end;

  -- delete all instances
  declare
    procedure Delete is
      procedure Delete (This : Problem_Reporting.Problem_Report.Handle;
                        OK : out Boolean) is
      begin
        Problem_Reporting.Problem_Report.Delete
           ((Id => Problem_Reporting.Problem_Report.Get_Id (This)));
        OK := True;
      end Delete;
      procedure Visit_All is
        new Problem_Reporting.Problem_Report.Abstract_Containers.Visit (Delete);
      All_Instances : Problem_Reporting.Problem_Report.Collections.Collection
         := Problem_Reporting.Problem_Report.All_Instances;
      It : Problem_Reporting.Problem_Report.Abstract_Containers.Iterator'Class
          := Problem_Reporting.Problem_Report.Collections.New_Iterator
             (All_Instances);
    begin
      Visit_All (It);
    end Delete;
  begin
    Delete;
  end;

  -- time inserting new instances
  declare
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
    Interval : Duration;
  begin
    for I in 101 .. 101 + Loops - 1 loop
      declare
        This : Problem_Reporting.Problem_Report.Handle;
      begin
        This := Problem_Reporting.Problem_Report.Create ((Id => I));
        Problem_Reporting.Problem_Report.Set_Details (This, +("detail" & I'Img));
        Problem_Reporting.Problem_Report.Set_Reporter (This, +"simon");
      end;
    end loop;
    Interval := (Ada.Calendar.Clock - Start) / Loops * 1_000_000; -- uS
    Put_Line ("inserting ->" & Interval'Img & " uS");
  end;

  -- time finding an instance by identifier
  declare
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
    Interval : Duration;
  begin
    for I in 101 .. 101 + 10_000 - 1 loop
      declare
        This : Problem_Reporting.Problem_Report.Handle;
      begin
        This := Problem_Reporting.Problem_Report.Find ((Id => I));
      end;
    end loop;
    Interval := (Ada.Calendar.Clock - Start) / Loops * 1_000_000; -- uS
    Put_Line ("finding ->" & Interval'Img & " uS");
  end;

  -- time getting all instances
  declare
    Interval : Duration;
    All_Instances : Problem_Reporting.Problem_Report.Collections.Collection;
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
  begin
    All_Instances := Problem_Reporting.Problem_Report.All_Instances;
    Interval := (Ada.Calendar.Clock - Start) * 1_000_000; -- uS
    Put_Line ("getting all instances ->" & Interval'Img & " uS");
    Put_Line
       ("result size"
        & Problem_Reporting.Problem_Report.Collections.Length (All_Instances)'Img);
  end;

  -- time selection (failure)
  declare
    function Reject
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean;
    function Filter is new Problem_Reporting.Problem_Report.Selection_Function
       (Pass => Reject);
    function Reject
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean is
    begin
      return False;
    end Reject;
    Interval : Duration;
    Result : Problem_Reporting.Problem_Report.Collections.Collection;
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
  begin
    Result := Filter;
    Interval := (Ada.Calendar.Clock - Start) / Loops * 1_000_000; -- uS
    Put_Line
       ("rejecting ->" & Interval'Img & " uS");
    Put_Line
       ("result size"
        & Problem_Reporting.Problem_Report.Collections.Length (Result)'Img);
  end;

  -- time selection (success)
  declare
    function Pass
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean;
    function Filter is new Problem_Reporting.Problem_Report.Selection_Function;
    function Pass
       (This : Problem_Reporting.Problem_Report.Handle) return Boolean is
    begin
      return True;
    end Pass;
    Interval : Duration;
    Result : Problem_Reporting.Problem_Report.Collections.Collection;
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
  begin
    Result := Filter;
    Interval := (Ada.Calendar.Clock - Start) / Loops * 1_000_000; -- uS
    Put_Line ("accepting ->" & Interval'Img & " uS");
    Put_Line
       ("result size"
        & Problem_Reporting.Problem_Report.Collections.Length (Result)'Img);
  end;

  -- time deletion
  declare
    Start : Ada.Calendar.Time := Ada.Calendar.Clock;
    Interval : Duration;
  begin
    for I in 101 .. 101 + 10_000 - 1 loop
       Problem_Reporting.Problem_Report.Delete ((Id => I));
    end loop;
    Interval := (Ada.Calendar.Clock - Start) / Loops * 1_000_000; -- uS
    Put_Line ("deleting ->" & Interval'Img & " uS");
  end;

end T;
