-- $Id: problem_reporting-component-test.ts,v e496bbe2447e 2001/04/22 10:49:46 simon $

error_handling continue

context with Ada.Text_IO; use Ada.Text_IO;
	with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
        with Architecture.Exceptions;
	with Problem_Reporting.Component; use Problem_Reporting.Component;

***** (1) Create
define 	H : Problem_Reporting.Component.Handle;
	N : Unbounded_String := To_Unbounded_String ("first");
test	H := Problem_Reporting.Component.Create ((Id => N));
pass	H /= null
cleanup Problem_Reporting.Component.Delete (H);

***** (2) Create twice
define 	H : Problem_Reporting.Component.Handle;
	N : Unbounded_String := To_Unbounded_String ("first");
test	H := Problem_Reporting.Component.Create ((Id => N));
	H := Problem_Reporting.Component.Create ((Id => N));
pass	exception Architecture.Exceptions.Duplicate
cleanup Problem_Reporting.Component.Delete (H);

***** (3) Find
define 	H, R : Problem_Reporting.Component.Handle;
	N : Unbounded_String := To_Unbounded_String ("first");
test	H := Problem_Reporting.Component.Create ((Id => N));
	R := Problem_Reporting.Component.Find ((Id => N));
pass	R = H
cleanup Problem_Reporting.Component.Delete (H);

***** (4) Find (two entries)
define 	H1, H2, R1, R2 : Problem_Reporting.Component.Handle;
	N1 : Unbounded_String := To_Unbounded_String ("first");
	N2 : Unbounded_String := To_Unbounded_String ("second");
test	H1 := Problem_Reporting.Component.Create ((Id => N1));
	H2 := Problem_Reporting.Component.Create ((Id => N2));
	R1 := Problem_Reporting.Component.Find ((Id => N1));
	R2 := Problem_Reporting.Component.Find ((Id => N2));
pass	R1 = H1 and R2 = H2
cleanup Problem_Reporting.Component.Delete (H1);
        Problem_Reporting.Component.Delete (H2);

***** (5) Find (two entries, but not this one)
define 	H1, H2, R1, R2, R3 : Problem_Reporting.Component.Handle;
	N1 : Unbounded_String := To_Unbounded_String ("first");
	N2 : Unbounded_String := To_Unbounded_String ("second");
	N3 : Unbounded_String := To_Unbounded_String ("third");
test	H1 := Problem_Reporting.Component.Create ((Id => N1));
	H2 := Problem_Reporting.Component.Create ((Id => N2));
	R1 := Problem_Reporting.Component.Find ((Id => N1));
	R2 := Problem_Reporting.Component.Find ((Id => N2));
	R3 := Problem_Reporting.Component.Find ((Id => N3));
pass	R1 = H1 and R2 = H2 and R3 = null
cleanup Problem_Reporting.Component.Delete (H1);
        Problem_Reporting.Component.Delete (H2);

***** (6) Delete a null entry
define 	H : Problem_Reporting.Component.Handle;
test	Problem_Reporting.Component.Delete (H);
pass	exception Constraint_Error

***** (7) Delete an element which doesn't exist
define  H : Problem_Reporting.Component.Handle;
	N1 : Unbounded_String := To_Unbounded_String ("first");
	N2 : Unbounded_String := To_Unbounded_String ("second");
test	H := Problem_Reporting.Component.Create ((Id => N1));
        Problem_Reporting.Component.Delete ((Id => N2));
pass	exception Architecture.Exceptions.Not_Found
cleanup Problem_Reporting.Component.Delete (H);
