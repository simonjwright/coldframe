with Ada.Text_IO;

with ColdFrame.Exceptions.Traceback;
with ColdFrame.Instances;

with Deletion.Track;
with Deletion.Command_Threat;
with Deletion.New_Threat;
with Deletion.Old_Threat;
with Deletion.Not_Threat;
with Deletion.Threat;

procedure Deletion.Test is
   Tr1, Tr2, Tr3 : Track.Handle;
   Thr : Threat.Handle;
   Nthr : New_Threat.Handle;
   Othr : Old_Threat.Handle;
   Nothr : Not_Threat.Handle;
   Cthr : Command_Threat.Handle;
begin

   Tr1 := Track.Create;
   Thr := Threat.Create;
   Nthr := New_Threat.Create
     ((R1_Parent => ColdFrame.Instances.Handle (Tr1),
       R2_Parent => ColdFrame.Instances.Handle (Thr)));

   Tr2 := Track.Create;
   Thr := Threat.Create;
   Othr := Old_Threat.Create
     ((R1_Parent => ColdFrame.Instances.Handle (Tr2),
       R2_Parent => ColdFrame.Instances.Handle (Thr)));

   Tr3 := Track.Create;
   Nothr := Not_Threat.Create
     ((R1_Parent => ColdFrame.Instances.Handle (Tr3)));

   Thr := Threat.Create;
   Cthr := Command_Threat.Create
     ((R2_Parent => ColdFrame.Instances.Handle (Thr)));

   Track.Delete_Threat (Tr1);
   Track.Delete_Threat (Tr2);
   Track.Delete_Threat (Tr3);
   Command_Threat.Delete_Threat (Cthr);

end Deletion.Test;
