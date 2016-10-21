pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2013 (20130314)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_hash_table_fun" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#37e878a3#;
   pragma Export (C, u00001, "hash_table_funB");
   u00002 : constant Version_32 := 16#3935bd10#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#51a8eec5#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#5ea2bd7b#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#6380a30f#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#16173147#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#1f42fb5e#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#5fc8ae56#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#0071025c#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#3293d48b#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#27940d94#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#e92aa296#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#17775d6d#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#4ba689f8#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#ace32e1e#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#a505d3ce#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#4f750b3b#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#fc6a127a#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#7b9f0bae#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#cea672f3#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#5665ab64#;
   pragma Export (C, u00022, "system__htableB");
   u00023 : constant Version_32 := 16#dc60e058#;
   pragma Export (C, u00023, "system__htableS");
   u00024 : constant Version_32 := 16#8b7dad61#;
   pragma Export (C, u00024, "system__string_hashB");
   u00025 : constant Version_32 := 16#795476c2#;
   pragma Export (C, u00025, "system__string_hashS");
   u00026 : constant Version_32 := 16#aad75561#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#533666e1#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#010db1dc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#67b88b82#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#b012ff50#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#5d134e94#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#dc8e33ed#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#3e4f7a23#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#907d882f#;
   pragma Export (C, u00034, "system__wch_conB");
   u00035 : constant Version_32 := 16#e023806b#;
   pragma Export (C, u00035, "system__wch_conS");
   u00036 : constant Version_32 := 16#22fed88a#;
   pragma Export (C, u00036, "system__wch_stwB");
   u00037 : constant Version_32 := 16#cd32ac6a#;
   pragma Export (C, u00037, "system__wch_stwS");
   u00038 : constant Version_32 := 16#617a40f2#;
   pragma Export (C, u00038, "system__wch_cnvB");
   u00039 : constant Version_32 := 16#fedd06bd#;
   pragma Export (C, u00039, "system__wch_cnvS");
   u00040 : constant Version_32 := 16#cb4a8015#;
   pragma Export (C, u00040, "interfacesS");
   u00041 : constant Version_32 := 16#75729fba#;
   pragma Export (C, u00041, "system__wch_jisB");
   u00042 : constant Version_32 := 16#aaaf9da9#;
   pragma Export (C, u00042, "system__wch_jisS");
   u00043 : constant Version_32 := 16#ada34a87#;
   pragma Export (C, u00043, "system__traceback_entriesB");
   u00044 : constant Version_32 := 16#0de94017#;
   pragma Export (C, u00044, "system__traceback_entriesS");
   u00045 : constant Version_32 := 16#e18a47a0#;
   pragma Export (C, u00045, "ada__float_text_ioB");
   u00046 : constant Version_32 := 16#e61b3c6c#;
   pragma Export (C, u00046, "ada__float_text_ioS");
   u00047 : constant Version_32 := 16#421d3150#;
   pragma Export (C, u00047, "ada__text_ioB");
   u00048 : constant Version_32 := 16#152cee1e#;
   pragma Export (C, u00048, "ada__text_ioS");
   u00049 : constant Version_32 := 16#1358602f#;
   pragma Export (C, u00049, "ada__streamsS");
   u00050 : constant Version_32 := 16#afd62b40#;
   pragma Export (C, u00050, "ada__tagsB");
   u00051 : constant Version_32 := 16#f6fc5406#;
   pragma Export (C, u00051, "ada__tagsS");
   u00052 : constant Version_32 := 16#3529f220#;
   pragma Export (C, u00052, "system__unsigned_typesS");
   u00053 : constant Version_32 := 16#79817c71#;
   pragma Export (C, u00053, "system__val_unsB");
   u00054 : constant Version_32 := 16#25811f1b#;
   pragma Export (C, u00054, "system__val_unsS");
   u00055 : constant Version_32 := 16#aea309ed#;
   pragma Export (C, u00055, "system__val_utilB");
   u00056 : constant Version_32 := 16#f36818a8#;
   pragma Export (C, u00056, "system__val_utilS");
   u00057 : constant Version_32 := 16#b7fa72e7#;
   pragma Export (C, u00057, "system__case_utilB");
   u00058 : constant Version_32 := 16#f2d4cede#;
   pragma Export (C, u00058, "system__case_utilS");
   u00059 : constant Version_32 := 16#e0b7a7e8#;
   pragma Export (C, u00059, "interfaces__c_streamsB");
   u00060 : constant Version_32 := 16#95ad191f#;
   pragma Export (C, u00060, "interfaces__c_streamsS");
   u00061 : constant Version_32 := 16#36e568f7#;
   pragma Export (C, u00061, "system__crtlS");
   u00062 : constant Version_32 := 16#228a5436#;
   pragma Export (C, u00062, "system__file_ioB");
   u00063 : constant Version_32 := 16#ce89cf71#;
   pragma Export (C, u00063, "system__file_ioS");
   u00064 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00064, "ada__finalizationB");
   u00065 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00065, "ada__finalizationS");
   u00066 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00066, "system__finalization_rootB");
   u00067 : constant Version_32 := 16#103addc6#;
   pragma Export (C, u00067, "system__finalization_rootS");
   u00068 : constant Version_32 := 16#b46168d5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00069, "interfaces__cB");
   u00070 : constant Version_32 := 16#29899d4e#;
   pragma Export (C, u00070, "interfaces__cS");
   u00071 : constant Version_32 := 16#507533cc#;
   pragma Export (C, u00071, "interfaces__c__stringsB");
   u00072 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00072, "interfaces__c__stringsS");
   u00073 : constant Version_32 := 16#d6bc4ecc#;
   pragma Export (C, u00073, "system__crtl__runtimeS");
   u00074 : constant Version_32 := 16#1eab0e09#;
   pragma Export (C, u00074, "system__img_enum_newB");
   u00075 : constant Version_32 := 16#d8cf65a6#;
   pragma Export (C, u00075, "system__img_enum_newS");
   u00076 : constant Version_32 := 16#f6ee85e9#;
   pragma Export (C, u00076, "system__os_libB");
   u00077 : constant Version_32 := 16#89dce9aa#;
   pragma Export (C, u00077, "system__os_libS");
   u00078 : constant Version_32 := 16#4cd8aca0#;
   pragma Export (C, u00078, "system__stringsB");
   u00079 : constant Version_32 := 16#e822e492#;
   pragma Export (C, u00079, "system__stringsS");
   u00080 : constant Version_32 := 16#782cc428#;
   pragma Export (C, u00080, "system__file_control_blockS");
   u00081 : constant Version_32 := 16#91d2300e#;
   pragma Export (C, u00081, "system__finalization_mastersB");
   u00082 : constant Version_32 := 16#353d027a#;
   pragma Export (C, u00082, "system__finalization_mastersS");
   u00083 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00083, "system__address_imageB");
   u00084 : constant Version_32 := 16#fe24336c#;
   pragma Export (C, u00084, "system__address_imageS");
   u00085 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00085, "system__img_boolB");
   u00086 : constant Version_32 := 16#aa11dfbd#;
   pragma Export (C, u00086, "system__img_boolS");
   u00087 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00087, "system__ioB");
   u00088 : constant Version_32 := 16#c18a5919#;
   pragma Export (C, u00088, "system__ioS");
   u00089 : constant Version_32 := 16#a7a37cb6#;
   pragma Export (C, u00089, "system__storage_poolsB");
   u00090 : constant Version_32 := 16#8c66b13b#;
   pragma Export (C, u00090, "system__storage_poolsS");
   u00091 : constant Version_32 := 16#ba5d60c7#;
   pragma Export (C, u00091, "system__pool_globalB");
   u00092 : constant Version_32 := 16#d56df0a6#;
   pragma Export (C, u00092, "system__pool_globalS");
   u00093 : constant Version_32 := 16#742a8355#;
   pragma Export (C, u00093, "system__memoryB");
   u00094 : constant Version_32 := 16#95431243#;
   pragma Export (C, u00094, "system__memoryS");
   u00095 : constant Version_32 := 16#1fd820b1#;
   pragma Export (C, u00095, "system__storage_pools__subpoolsB");
   u00096 : constant Version_32 := 16#951e0de9#;
   pragma Export (C, u00096, "system__storage_pools__subpoolsS");
   u00097 : constant Version_32 := 16#1777d351#;
   pragma Export (C, u00097, "system__storage_pools__subpools__finalizationB");
   u00098 : constant Version_32 := 16#12aaf1de#;
   pragma Export (C, u00098, "system__storage_pools__subpools__finalizationS");
   u00099 : constant Version_32 := 16#d5f9759f#;
   pragma Export (C, u00099, "ada__text_io__float_auxB");
   u00100 : constant Version_32 := 16#f854caf5#;
   pragma Export (C, u00100, "ada__text_io__float_auxS");
   u00101 : constant Version_32 := 16#cd6ba629#;
   pragma Export (C, u00101, "ada__text_io__generic_auxB");
   u00102 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00102, "ada__text_io__generic_auxS");
   u00103 : constant Version_32 := 16#6d0081c3#;
   pragma Export (C, u00103, "system__img_realB");
   u00104 : constant Version_32 := 16#9860ffb4#;
   pragma Export (C, u00104, "system__img_realS");
   u00105 : constant Version_32 := 16#80f37066#;
   pragma Export (C, u00105, "system__fat_llfS");
   u00106 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00106, "system__float_controlB");
   u00107 : constant Version_32 := 16#bf34ed6a#;
   pragma Export (C, u00107, "system__float_controlS");
   u00108 : constant Version_32 := 16#06417083#;
   pragma Export (C, u00108, "system__img_lluB");
   u00109 : constant Version_32 := 16#7ce0f2e3#;
   pragma Export (C, u00109, "system__img_lluS");
   u00110 : constant Version_32 := 16#194ccd7b#;
   pragma Export (C, u00110, "system__img_unsB");
   u00111 : constant Version_32 := 16#aaddced7#;
   pragma Export (C, u00111, "system__img_unsS");
   u00112 : constant Version_32 := 16#0fb8c821#;
   pragma Export (C, u00112, "system__powten_tableS");
   u00113 : constant Version_32 := 16#8ff77155#;
   pragma Export (C, u00113, "system__val_realB");
   u00114 : constant Version_32 := 16#a1e1d947#;
   pragma Export (C, u00114, "system__val_realS");
   u00115 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00115, "system__exn_llfB");
   u00116 : constant Version_32 := 16#de4cb0b9#;
   pragma Export (C, u00116, "system__exn_llfS");
   u00117 : constant Version_32 := 16#dc11d781#;
   pragma Export (C, u00117, "system__fat_fltS");
   u00118 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00118, "ada__integer_text_ioB");
   u00119 : constant Version_32 := 16#f1daf268#;
   pragma Export (C, u00119, "ada__integer_text_ioS");
   u00120 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00120, "ada__text_io__integer_auxB");
   u00121 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00121, "ada__text_io__integer_auxS");
   u00122 : constant Version_32 := 16#ef6c8032#;
   pragma Export (C, u00122, "system__img_biuB");
   u00123 : constant Version_32 := 16#f30b7a6d#;
   pragma Export (C, u00123, "system__img_biuS");
   u00124 : constant Version_32 := 16#10618bf9#;
   pragma Export (C, u00124, "system__img_llbB");
   u00125 : constant Version_32 := 16#b2cc6a93#;
   pragma Export (C, u00125, "system__img_llbS");
   u00126 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00126, "system__img_lliB");
   u00127 : constant Version_32 := 16#4e87fb87#;
   pragma Export (C, u00127, "system__img_lliS");
   u00128 : constant Version_32 := 16#f931f062#;
   pragma Export (C, u00128, "system__img_llwB");
   u00129 : constant Version_32 := 16#1ba04905#;
   pragma Export (C, u00129, "system__img_llwS");
   u00130 : constant Version_32 := 16#b532ff4e#;
   pragma Export (C, u00130, "system__img_wiuB");
   u00131 : constant Version_32 := 16#9d4afdff#;
   pragma Export (C, u00131, "system__img_wiuS");
   u00132 : constant Version_32 := 16#c31442ce#;
   pragma Export (C, u00132, "system__val_intB");
   u00133 : constant Version_32 := 16#176d8469#;
   pragma Export (C, u00133, "system__val_intS");
   u00134 : constant Version_32 := 16#d3757657#;
   pragma Export (C, u00134, "system__val_lliB");
   u00135 : constant Version_32 := 16#c5ec48f6#;
   pragma Export (C, u00135, "system__val_lliS");
   u00136 : constant Version_32 := 16#25c21d28#;
   pragma Export (C, u00136, "system__val_lluB");
   u00137 : constant Version_32 := 16#4fdba552#;
   pragma Export (C, u00137, "system__val_lluS");
   u00138 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00138, "ada__stringsS");
   u00139 : constant Version_32 := 16#261c554b#;
   pragma Export (C, u00139, "ada__strings__unboundedB");
   u00140 : constant Version_32 := 16#2bf53506#;
   pragma Export (C, u00140, "ada__strings__unboundedS");
   u00141 : constant Version_32 := 16#b490d2ed#;
   pragma Export (C, u00141, "ada__strings__searchB");
   u00142 : constant Version_32 := 16#b5a8c1d6#;
   pragma Export (C, u00142, "ada__strings__searchS");
   u00143 : constant Version_32 := 16#96e9c1e7#;
   pragma Export (C, u00143, "ada__strings__mapsB");
   u00144 : constant Version_32 := 16#24318e4c#;
   pragma Export (C, u00144, "ada__strings__mapsS");
   u00145 : constant Version_32 := 16#d7ba84a5#;
   pragma Export (C, u00145, "system__bit_opsB");
   u00146 : constant Version_32 := 16#c30e4013#;
   pragma Export (C, u00146, "system__bit_opsS");
   u00147 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00147, "ada__charactersS");
   u00148 : constant Version_32 := 16#051b1b7b#;
   pragma Export (C, u00148, "ada__characters__latin_1S");
   u00149 : constant Version_32 := 16#c4857ee1#;
   pragma Export (C, u00149, "system__compare_array_unsigned_8B");
   u00150 : constant Version_32 := 16#85f3589b#;
   pragma Export (C, u00150, "system__compare_array_unsigned_8S");
   u00151 : constant Version_32 := 16#9d3d925a#;
   pragma Export (C, u00151, "system__address_operationsB");
   u00152 : constant Version_32 := 16#9fb647c1#;
   pragma Export (C, u00152, "system__address_operationsS");
   u00153 : constant Version_32 := 16#5073d598#;
   pragma Export (C, u00153, "system__machine_codeS");
   u00154 : constant Version_32 := 16#8d43fb6a#;
   pragma Export (C, u00154, "system__atomic_countersB");
   u00155 : constant Version_32 := 16#1554c2e2#;
   pragma Export (C, u00155, "system__atomic_countersS");
   u00156 : constant Version_32 := 16#a6e358bc#;
   pragma Export (C, u00156, "system__stream_attributesB");
   u00157 : constant Version_32 := 16#e89b4b3f#;
   pragma Export (C, u00157, "system__stream_attributesS");
   u00158 : constant Version_32 := 16#54b8a3b7#;
   pragma Export (C, u00158, "ada__text_io__unbounded_ioS");
   u00159 : constant Version_32 := 16#97a2d3b4#;
   pragma Export (C, u00159, "ada__strings__unbounded__text_ioB");
   u00160 : constant Version_32 := 16#2124c8bb#;
   pragma Export (C, u00160, "ada__strings__unbounded__text_ioS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.img_real%s
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.atomic_counters%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_real%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.val_int%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  interfaces.c.strings%s
   --  system.crtl.runtime%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.secondary_stack%s
   --  system.file_io%b
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c.strings%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.strings.unbounded.text_io%s
   --  ada.strings.unbounded.text_io%b
   --  ada.text_io.float_aux%s
   --  ada.float_text_io%s
   --  ada.float_text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.float_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  ada.text_io.unbounded_io%s
   --  hash_table_fun%b
   --  END ELABORATION ORDER


end ada_main;
