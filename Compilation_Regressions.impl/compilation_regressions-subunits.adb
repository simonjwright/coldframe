--  $RCSfile: compilation_regressions-subunits.adb,v $
--  $Revision: 5fca6c88686e $
--  $Date: 2007/10/27 15:51:38 $
--  $Author: simonjwright $
--
--  Regression tests for ColdFrame.
--
--  The with'ed units only have to compile.

pragma Warnings (Off);
with Compilation_Regressions.A_Singleton;
with Compilation_Regressions.A_To_B;
with Compilation_Regressions.Aliased_Components;
with Compilation_Regressions.Bounded_String_ID;
with Compilation_Regressions.CT_Class;
with Compilation_Regressions.Child_Uses_Action;
with Compilation_Regressions.Child_With_Private_Operations;
with Compilation_Regressions.Class_A_In_Association;
with Compilation_Regressions.Class_B_In_Association;
with Compilation_Regressions.Class_Operation_Access_Modes;
with Compilation_Regressions.Class_Timer_In_Array;
with Compilation_Regressions.Class_Timer_In_Class;
with Compilation_Regressions.Class_Timer_In_Max_One;
with Compilation_Regressions.Class_Timer_In_Public;
with Compilation_Regressions.Class_Timer_In_Singleton;
with Compilation_Regressions.Class_To_Be_Associated;
with Compilation_Regressions.Class_With_Class_Variable;
with Compilation_Regressions.Class_With_Private_Operations;
with Compilation_Regressions.Cls_With_Atomic_And_Volatile_Component;
with Compilation_Regressions.C_Fixed_String;
with Compilation_Regressions.C_U1;
with Compilation_Regressions.Function_Returning_Handle;
with Compilation_Regressions.Grandchild_Supplied_Actual;
with Compilation_Regressions.Identified_Class;
with Compilation_Regressions.Long_Modular_Ident;
with Compilation_Regressions.One_Enum_ID;
with Compilation_Regressions.One_Int_ID;
with Compilation_Regressions.One_To_One;
with Compilation_Regressions.Ordinary_Class.All_Instances;
with Compilation_Regressions.Ordinary_Class.Selection_Function;
with Compilation_Regressions.Parent_With_Action;
with Compilation_Regressions.Parent_With_Private_Operations;
with Compilation_Regressions.Public_With_Attributes;
with Compilation_Regressions.Public_Without_Attributes;
with Compilation_Regressions.Renaming_Operations_Child;
with Compilation_Regressions.Renaming_Operations_Parent;
with Compilation_Regressions.S_E_E;
with Compilation_Regressions.Serializable;
with Compilation_Regressions.Service;
with Compilation_Regressions.Service_History;
with Compilation_Regressions.Short_Modular_Ident;
--  Doesn't work yet (23.iv.06).
--  with Compilation_Regressions.Singleton_Assoc;
with Compilation_Regressions.Singleton_With_Attributes;
with Compilation_Regressions.Singleton_With_Referential_Attribute;
with Compilation_Regressions.Singleton_Without_Attributes;
with Compilation_Regressions.Utility;
with Compilation_Regressions.Van;

package body Compilation_Regressions.Subunits is
end Compilation_Regressions.Subunits;
