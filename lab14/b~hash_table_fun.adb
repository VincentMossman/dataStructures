pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~hash_table_fun.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~hash_table_fun.adb");
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "system__fat_flt_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__fat_llf_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "ada__strings_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "ada__strings__maps_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "ada__tags_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__streams_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "interfaces__c_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "interfaces__c__strings_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "system__finalization_root_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__finalization_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__storage_pools_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "system__finalization_masters_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__storage_pools__subpools_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "system__pool_global_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__file_control_block_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "system__file_io_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "ada__strings__unbounded_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "ada__text_io_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E048 := E048 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__text_io__finalize_spec");
      begin
         F1;
      end;
      E140 := E140 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__strings__unbounded__finalize_spec");
      begin
         F2;
      end;
      E082 := E082 - 1;
      E096 := E096 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__file_io__finalize_body");
      begin
         E063 := E063 - 1;
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__file_control_block__finalize_spec");
      begin
         E080 := E080 - 1;
         F4;
      end;
      E092 := E092 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__pool_global__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__storage_pools__subpools__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__finalization_masters__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");
   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Fat_Flt'Elab_Spec;
      E117 := E117 + 1;
      System.Fat_Llf'Elab_Spec;
      E105 := E105 + 1;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E138 := E138 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E049 := E049 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Finalization_Root'Elab_Spec;
      E067 := E067 + 1;
      Ada.Finalization'Elab_Spec;
      E065 := E065 + 1;
      System.Storage_Pools'Elab_Spec;
      E090 := E090 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E092 := E092 + 1;
      System.File_Control_Block'Elab_Spec;
      E080 := E080 + 1;
      System.File_Io'Elab_Body;
      E063 := E063 + 1;
      E096 := E096 + 1;
      System.Finalization_Masters'Elab_Body;
      E082 := E082 + 1;
      E072 := E072 + 1;
      E070 := E070 + 1;
      Ada.Tags'Elab_Body;
      E051 := E051 + 1;
      E144 := E144 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E140 := E140 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E048 := E048 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_hash_table_fun");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   .\hash_table_fun.o
   --   -L.\
   --   -LC:\Users\mossmanv\Desktop\Lab14\
   --   -LC:\LANG\GNAT08\JEWL-17\source\
   --   -LC:/GNAT/2013/lib/gcc/i686-pc-mingw32/4.7.4/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
