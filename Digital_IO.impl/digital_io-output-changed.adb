--  $Id: digital_io-output-changed.adb,v c837ef8247ad 2003/01/07 20:10:08 simon $

--  Called when the signal has changed; no action for output signals
--  (the HCI polls).

separate (Digital_IO.Output)
procedure Changed
  (This : Handle) is
begin

   null;

end Changed;
