**&--------------------------------------------------------------------*
*& Z_IDR_SRYA_EXTRACTOR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&                        LICENSE CONDITIONS                           *
*                                                                      *
*  Z_IDR_SRYA_EXTRACTOR : IDR Scan Roles y Autorizaciones              *
*                                                                      *
*  Copyright (C) 2020 Novis Euforia                                    *
*                                                                      *
*  This program is free software: you can redistribute it and/or modi- *
*  fy it under the terms of the GNU Affero General Public License as   *
*  published by the Free Software Foundation, either version 3 of the  *
*  License, or (at your option) any later version.                     *
*                                                                      *
*  This program is distributed in the hope that it will be useful, but *
*  WITHOUT ANY WARRANTY; without even the implied warranty of          *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the        *
*  GNU Affero General Public License for more                          *
*  details.                                                            *
*                                                                      *
*  You should have received a copy of the GNU Affero General Public    *
*  License along with this program. If not, see                        *
*  <https://www.gnu.org/licenses/>.                                    *
*                                                                      *
*              https://www.gnu.org/licenses/agpl-3.0.txt               *
*                                                                      *
*&---------------------------------------------------------------------*
REPORT z_idr_srya_extractor.
************************************************************************
*  2020.03.19 : SRYA EXTRACTOR V1.0 : First Release
*  2020.04.03 : SRYA EXTRACTOR V1.1 : CSV generation
*  2020.05.19 : SRYA EXTRACTOR V1.2 : TRXS extraction
*  2020.07.08 : SRYA EXTRACTOR V1.2a: User code shake
*  2020.07.09 : SRYA EXTRACTOR V1.2b: File name normalization
*  2020.07.09 : SRYA EXTRACTOR V1.2b: Selection Screen text-elements
*                                     included un source-code
*  2020.07.09 : SRYA EXTRACTOR V1.2c: Customer Id file name generation
*  2020.07.10 : SRYA EXTRACTOR V1.3 : User Anonimization
*  2020.07.12 : SRYA EXTRACTOR V1.3 : User License data
************************************************************************
*-$- TYPES
TYPES: BEGIN OF t_extract01,
         tasktype  TYPE swnctasktyperaw,
         account   TYPE swncuname,
         tasktdesc TYPE swnctasktype,
         entry_id  TYPE swncentryid,
         count     TYPE swnccntagg,
       END OF t_extract01.

TYPES: BEGIN OF t_extract02,
         tasktype(1),
         account     TYPE swncuname,
         tasktdesc   TYPE swnctasktype,
         entry_id    TYPE swncentryid,
         count(24),
       END OF t_extract02.

TYPES: BEGIN OF t_trxs,
         tcode       TYPE tcode,
         progname    TYPE program_id,
         description TYPE ttext_stct,
       END OF t_trxs.

TYPES:BEGIN OF t_user,
        mandt         TYPE symandt,
        bname         TYPE xubname,
        Persnum       TYPE ad_persnum,
        name_last     TYPE ad_namelas,
        name_text     TYPE ad_namtext,
        mc_name_first TYPE ad_mc_nmfi,
        mc_name_last  TYPE ad_mc_nmla,
        gltgv         TYPE xugltgv,
        gltgb         TYPE xugltgb,
        trdat         TYPE xuldate,
        ltime         TYPE xultime,
        uflag         TYPE string,
      END OF t_user.


TYPES: BEGIN OF t_ulock,
         bname TYPE xubname,
         gltgv TYPE xugltgv,
         gltgb TYPE xugltgb,
         trdat TYPE xuldate,
         ltime TYPE xultime,
         uflag TYPE xuuflag,
       END OF t_ulock.

TYPES: BEGIN OF t_ushake,
         bname TYPE xubname,
         alias TYPE xubname,
       END OF t_ushake.


TYPES: tt_ushake TYPE STANDARD TABLE OF t_ushake.


TYPES: BEGIN OF t_userlic,
         usr06       TYPE usr06,
         sscr_allow  TYPE flag,
         active      TYPE rsuvmflag,
         sondervers  TYPE rsuvmflag,
         country     TYPE rsuvmflag,
         charge_info TYPE slim_charge_info_litype,
         utyptext    TYPE utyptext,
         sort        TYPE rsuvmsort,
       END OF t_userlic.

TYPES: BEGIN OF t_file,
         file TYPE  localfile,
       END OF t_file.


*$-Local Classes

CLASS lcl_winpath DEFINITION.
  PUBLIC SECTION.

    METHODS constructor IMPORTING i_file TYPE rlgrap-filename.
    METHODS get_winpath RETURNING VALUE(l_winpath) TYPE localfile.

  PRIVATE SECTION.

    DATA: l_file   TYPE Localfile,
          lo_winfs TYPE REF TO cl_fs_windows_path.

ENDCLASS.

CLASS lcl_ushaker DEFINITION.
  PUBLIC SECTION.


    METHODS constructor.
    METHODS shake RETURNING VALUE(alias) TYPE xubname.
    METHODS get_alias IMPORTING bname TYPE xubname EXPORTING alias TYPE xubname.
    METHODS get_alias_table EXPORTING lt_ushake TYPE tt_ushake .

  PRIVATE SECTION.

    DATA: l_prefix   TYPE string,
          l_sufix(5) TYPE n,
          last_alias TYPE xubname.
    DATA: lt_ushake   TYPE tt_ushake.

ENDCLASS.
*$-
CLASS lcl_winpath IMPLEMENTATION.
  METHOD constructor.

    me->l_file = i_file.

  ENDMETHOD.                    "constructor

  METHOD get_winpath.

    me->lo_winfs =  cl_fs_windows_path=>create_windows_path( name = me->l_file ).
    l_winpath = lo_winfs->get_path_name( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_ushaker IMPLEMENTATION.

  METHOD constructor.

    FIELD-SYMBOLS: <fs_ushake> TYPE t_ushake.

    CONCATENATE  sy-sysid sy-mandt INTO me->l_prefix.
    me->l_sufix  = '00000'.
    SELECT  bname FROM usr02  INTO  TABLE me->lt_ushake.
    LOOP AT lt_ushake ASSIGNING <fs_ushake>.
      <fs_ushake>-alias = me->shake( ).
    ENDLOOP.

  ENDMETHOD.

  METHOD shake.

    me->l_sufix = me->l_sufix + 1.
    CONCATENATE me->l_prefix '_' me->l_sufix(5) INTO alias.

  ENDMETHOD.

  METHOD get_alias.
    DATA: lw_ushake TYPE t_ushake.

    alias = bname.

    READ TABLE me->lt_ushake INTO lw_ushake WITH KEY bname = bname.
    IF sy-subrc = 0.
      alias = lw_ushake-alias.
    ENDIF.
  ENDMETHOD.

  METHOD get_alias_table.

    lt_ushake = me->lt_ushake.

  ENDMETHOD.

ENDCLASS.

*-$- DATA
DATA: lt_ucode     TYPE STANDARD TABLE OF swncaggusertcode,
      lw_ucode     TYPE swncaggusertcode,
      lt_extract01 TYPE STANDARD TABLE OF  t_extract01,
      lw_extract01 TYPE  t_extract01,
      lt_extract02 TYPE STANDARD TABLE OF  t_extract02,
      lw_extract02 TYPE  t_extract02.
DATA: lw_agr_user   TYPE agr_users,
      lw_agr_define TYPE agr_define,
      lW_agr_texts  TYPE agr_texts,
      lw_agr_1251   TYPE agr_1251,
      lw_agr_prof   TYPE agr_prof,
      lw_usr01      TYPE usr01,
      lt_agr_user   TYPE STANDARD TABLE OF agr_users,
      lt_agr_define TYPE STANDARD TABLE OF agr_define,
      lt_agr_texts  TYPE STANDARD TABLE OF agr_texts,
      lt_agr_1251   TYPE STANDARD TABLE OF agr_1251,
      lt_agr_prof   TYPE STANDARD TABLE OF agr_prof,
      lt_username   TYPE STANDARD TABLE OF v_username,
      lw_username   TYPE v_username,
      lt_user       TYPE STANDARD TABLE OF t_user,
      lw_user       TYPE t_user,
      lt_ulock      TYPE STANDARD TABLE OF t_ulock,
      lw_ulock      TYPE t_ulock,
      lt_ushake     TYPE STANDARD TABLE OF t_ushake,
      lw_ushake     TYPE t_ushake,
      lt_tstcv      TYPE STANDARD TABLE OF tstcv,
      lw_tstcv      TYPE tstcv,
      lt_tstc       TYPE STANDARD TABLE OF tstc,
      lw_tstc       TYPE tstc,
      lt_trxs       TYPE STANDARD TABLE OF t_trxs,
      lw_trxs       TYPE t_trxs,
      lw_userlic    TYPE t_userlic,
      lw_usr06      TYPE usr06,
      lt_usr06      TYPE STANDARD TABLE OF usr06,
      lw_tutypa     TYPE tutypa,
      lt_tutypa     TYPE STANDARD TABLE OF tutypa,
      lw_tutypnow   TYPE tutypnow,
      lt_tutypnow   TYPE STANDARD TABLE OF tutypnow.

DATA: line_file01 TYPE string,
      pname(80).

DATA: ts          TYPE timestamp,
      s_ts        TYPE string,
      s_st_aux    TYPE string,
      s_st_aux2   TYPE string,
      l_firstlast TYPE d.

DATA: lw_opsystem TYPE opsystem,
      l_lifesWin  TYPE filesys_d VALUE 'WINDOWS NT',
      lc_winpath  TYPE REF TO lcl_winpath,
      lw_file     TYPE t_file,
      lt_files    TYPE STANDARD TABLE OF t_file.

DATA: lw_line TYPE string,
      lt_line TYPE STANDARD TABLE OF string.

DATA: lc_ushaker   TYPE REF TO lcl_ushaker,
      l_fileprefix TYPE string.



*-$-SELECTION SCREEN

SELECTION-SCREEN BEGIN OF BLOCK cli WITH FRAME TITLE title4.
  PARAMETERS: p_custo TYPE text12 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK cli.


SELECTION-SCREEN BEGIN OF BLOCK use WITH FRAME TITLE title0.
  PARAMETERS: p_use    AS CHECKBOX DEFAULT 'X',
              p_comp   TYPE swnchostname DEFAULT 'TOTAL' NO-DISPLAY,
              p_periot TYPE swncperitype DEFAULT 'M',
              p_perios TYPE swncdatum DEFAULT sy-datum,
              p_summar TYPE swnc_cflag DEFAULT '' NO-DISPLAY,
              p_file   TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt'.
SELECTION-SCREEN END OF BLOCK use.
SELECTION-SCREEN BEGIN OF BLOCK rol WITH FRAME TITLE title1.
  PARAMETERS: p_role   AS   CHECKBOX DEFAULT '',
              p_filer  TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt',
              p_filer2 TYPE rlgrap-filename  DEFAULT '/tmp/test02.txt',
              p_spra1  TYPE spras DEFAULT sy-langu NO-DISPLAY,
              p_filer3 TYPE rlgrap-filename  DEFAULT '/tmp/test03.txt',
              p_filer4 TYPE rlgrap-filename  DEFAULT '/tmp/test04.txt',
              p_filer5 TYPE rlgrap-filename  DEFAULT '/tmp/test05.txt'.
*              p_ushake TYPE flag DEFAULT '',
*              p_filer6 TYPE rlgrap-filename  DEFAULT '/tmp/alias.txt'.
SELECTION-SCREEN END OF BLOCK rol.

SELECTION-SCREEN BEGIN OF BLOCK lic WITH FRAME TITLE title5.
  PARAMETERS: p_licens TYPE flag DEFAULT '',
              p_filer7 TYPE rlgrap-filename  DEFAULT '/tmp/license.txt'.
SELECTION-SCREEN END OF BLOCK lic.

SELECTION-SCREEN BEGIN OF BLOCK gen WITH FRAME TITLE title2.
  PARAMETERS: p_gene   AS CHECKBOX DEFAULT '',
              p_spras  TYPE spras DEFAULT 'E' NO-DISPLAY,
              p_fileg1 TYPE rlgrap-filename  DEFAULT '/tmp/test06.txt'.
SELECTION-SCREEN END OF BLOCK gen.

SELECTION-SCREEN BEGIN OF BLOCK uan WITH FRAME TITLE title3.
  PARAMETERS: p_ushake TYPE flag DEFAULT '',
              p_filer6 TYPE rlgrap-filename  DEFAULT '/tmp/alias.txt'.
SELECTION-SCREEN END OF BLOCK uan.

*SELECTION-SCREEN ULINE /1(50).
SELECTION-SCREEN COMMENT /10(50) l_instr.
*SELECTION-SCREEN ULINE /1(50).


*AT SELECTION-SCREEN.

AT SELECTION-SCREEN.

  CLEAR l_fileprefix.
  CONCATENATE p_custo '_' sy-sysid '_' sy-mandt INTO l_fileprefix.

  CONCATENATE   '/tmp/' l_fileprefix '_USE_' p_comp '_' p_periot '_' p_perios'.csv' INTO p_file.
  lw_file-file = p_file. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_US_' s_ts '.csv' INTO p_filer.
  lw_file-file = p_filer. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_DE_' s_ts '.csv' INTO p_filer2.
  lw_file-file = p_filer2. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_12_' s_ts '.csv' INTO p_filer3.
  lw_file-file = p_filer3. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_PR_' s_ts '.csv' INTO p_filer4.
  lw_file-file = p_filer4. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_USE_GEN_DA'  s_ts '.csv' INTO p_filer5.
  lw_file-file = p_filer5. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_GEN_TRXS_'  s_ts '.csv' INTO p_fileg1.
  lw_file-file = p_fileg1. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ALIAS_'  s_ts '.csv' INTO p_filer6.
  lw_file-file = p_filer6. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_LIC_USERS_' s_ts '.csv'  INTO p_filer7.
  lw_file-file = p_filer7. APPEND lw_file TO lt_files.

*-$- SELSCR
INITIALIZATION.

  l_firstlast            =   sy-datum.

  IF l_firstlast+4(2)    <>  '01'.
    l_firstlast+4(2)    =   l_firstlast+4(2) - 1.
  ELSE.
    l_firstlast+4(2)     =  '12'.
    l_firstlast(4)       =   l_firstlast(4) - 1.
  ENDIF.
  l_firstlast+6(2)    =   '01'.

  p_perios = l_firstlast.

  %_P_COMP_%_app_%-text   = 'Component'.
  %_P_CUSTO_%_app_%-text  = 'Customer Name'.
  %_P_FILE_%_app_%-text   = 'Use Data File'.
  %_P_FILEG1_%_app_%-text = 'Transaction Data File'.
  %_P_FILER_%_app_%-text  = 'AGR Roles Data File'.
  %_P_FILER2_%_app_%-text = 'AGR Define Data File'.
  %_P_FILER3_%_app_%-text = 'AGR 1251 Data File'.
  %_P_FILER4_%_app_%-text = 'AGR Prof Data File'.
  %_P_FILER5_%_app_%-text = 'USER General Data File'.
  %_P_FILER6_%_app_%-text = 'Alias Data File'.
  %_P_GENE_%_app_%-text   = 'General Data Generation'.
  %_P_PERIOS_%_app_%-text = 'Date'.
  %_P_PERIOT_%_app_%-text = 'Period'.
  %_P_ROLE_%_app_%-text   = 'R & U Data Generation'.
  %_P_SPRA1_%_app_%-text  = 'Roles Description Language'.
  %_P_SUMMAR_%_app_%-text = 'Summary Only'.
  %_P_USE_%_app_%-text    = 'Use Data Generation'.
  %_P_USHAKE_%_app_%-text = 'User Alias Generation'.
  %_P_LICENS_%_app_%-text = 'License Data Generation'.
  %_P_FILER7_%_app_%-text = 'User License Data File'.

  title0 = 'Use'.
  title1 = 'Roles & Users'.
  title2 = 'General'.
  title3 = 'User Anonymization'.
  title4 = 'Customer'.
  title5 = 'Licenses'.

  GET TIME STAMP FIELD ts.
  s_ts = ts.
  CONDENSE s_ts.

* CLEAR p_file.
*  CONCATENATE:  '/tmp/Use_' p_comp '_' p_periot '_' p_perios'.txt' INTO p_file.

  CONCATENATE p_custo '_' sy-sysid '_' sy-mandt INTO l_fileprefix.

  CONCATENATE   '/tmp/' l_fileprefix '_USE_' p_comp '_' p_periot '_' p_perios'.csv' INTO p_file.
  lw_file-file = p_file. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_US_' s_ts '.csv' INTO p_filer.
  lw_file-file = p_filer. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_DE_' s_ts '.csv' INTO p_filer2.
  lw_file-file = p_filer2. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_12_' s_ts '.csv' INTO p_filer3.
  lw_file-file = p_filer3. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ROL_AGR_PR_' s_ts '.csv' INTO p_filer4.
  lw_file-file = p_filer4. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_USE_GEN_DA'  s_ts '.csv' INTO p_filer5.
  lw_file-file = p_filer5. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_GEN_TRXS_'  s_ts '.csv' INTO p_fileg1.
  lw_file-file = p_fileg1. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_ALIAS_'  s_ts '.csv' INTO p_filer6.
  lw_file-file = p_filer6. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/' l_fileprefix '_LIC_USERS_' s_ts '.csv'  INTO p_filer7.
  lw_file-file = p_filer7. APPEND lw_file TO lt_files.

*  Condense: p_file, p_filer

  SELECT SINGLE * FROM opsystem INTO lw_opsystem WHERE opsys = sy-opsys.

  lw_opsystem-filesys = to_upper( lw_opsystem-filesys ).

  IF lw_opsystem-filesys EQ l_lifesWin.

    LOOP AT lt_files INTO lw_file.
      CREATE OBJECT lc_winpath
        EXPORTING
          i_file = lw_file-file.

      CASE sy-tabix.
        WHEN 1.
          p_file   = lc_winpath->get_winpath( ).
        WHEN 2.
          p_filer  = lc_winpath->get_winpath( ).
        WHEN 3.
          p_filer2 = lc_winpath->get_winpath( ).
        WHEN 4.
          p_filer3 = lc_winpath->get_winpath( ).
        WHEN 5.
          p_filer4 = lc_winpath->get_winpath( ).
        WHEN 6.
          p_filer5 = lc_winpath->get_winpath( ).
        WHEN 7.
          p_fileg1 = lc_winpath->get_winpath( ).
        WHEN 8.
          p_filer6 = lc_winpath->get_winpath( ).
      ENDCASE.

      FREE lc_winpath.
    ENDLOOP.

  ENDIF.

  l_instr = 'Copyright © 2020 Novis Euforia'.

START-OF-SELECTION.

*------------------------------------------------------------------------&UAN
  IF p_ushake  = 'X'.
    CREATE OBJECT lc_ushaker.
  ENDIF.

  IF p_ushake = 'X'.

    OPEN DATASET p_filer6 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    CALL METHOD lc_ushaker->get_alias_table IMPORTING lt_ushake = lt_ushake.

    LOOP AT lt_ushake INTO lw_ushake.

      CONCATENATE lw_ushake-bname ';' lw_ushake-alias INTO lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer6.
      CLEAR: line_file01,lw_line.

    ENDLOOP.
    CLOSE DATASET p_filer6.
    WRITE: / 'File' ,p_filer6 ,  ' has been generated'.
    CLEAR lt_ushake[].
  ENDIF.

*------------------------------------------------------------------------&USE
  IF p_use IS NOT INITIAL.

    CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
      EXPORTING
        component     = p_comp
*       ASSIGNDSYS    = SY-SYSID
        periodtype    = p_periot
        periodstrt    = p_perios
        summary_only  = p_summar
*       STORAGE_TYPE  = ' '
*       FACTOR        = 1000
      TABLES
*       TASKTYPE      =
*       TASKTIMES     =
*       TIMES         =
*       DBPROCS       =
*       EXTSYSTEM     =
*       TCDET         =
*       FRONTEND      =
*       MEMORY        =
*       SPOOLACT      =
*       TABLEREC      =
        usertcode     = lt_ucode
*       USERWORKLOAD  =
*       RFCCLNT       =
*       RFCCLNTDEST   =
*       RFCSRVR       =
*       RFCSRVRDEST   =
*       SPOOL         =
*       HITLIST_DATABASE       =
*       HITLIST_RESPTIME       =
*       ASTAT         =
*       ASHITL_DATABASE        =
*       ASHITL_RESPTIME        =
*       COMP_HIERARCHY         =
*       ORG_UNITS     =
*       DBCON         =
*       VMC           =
*       WEBSD         =
*       WEBCD         =
*       WEBS          =
*       WEBC          =
*       TREX          =
*       FE            =
      EXCEPTIONS
        no_data_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE 'No data selected'(001) TYPE 'E'.
    ENDIF.

    DATA: l_entry_id1(40),
          l_entry_id2(32),
          l_entry_id3(1).

    IF lt_ucode IS INITIAL.
      WRITE: / 'File' ,p_file ,  ' has not been generated' COLOR COL_NEGATIVE .
    ELSE.
      LOOP AT lt_ucode INTO lw_ucode.
        lw_extract01-tasktype = lw_ucode-tasktype.
        lw_extract01-account  = lw_ucode-account.
        lw_extract01-entry_id = lw_ucode-entry_id.
        lw_extract01-count    = lw_ucode-count.

        CALL METHOD cl_swnc_collector_info=>translate_tasktype
          EXPORTING
            tasktyperaw = lw_ucode-tasktype
          RECEIVING
            tasktype    = lw_extract01-tasktdesc.
        APPEND lw_extract01 TO lt_extract01.
      ENDLOOP.

      OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      "SKIPPING BYTE-ORDER MARK.
      LOOP AT lt_extract01 INTO lw_extract01.

        lw_extract02-tasktype   = lw_extract01-tasktype.
        lw_extract02-account    = lw_extract01-account.
        lw_extract02-tasktdesc  = lw_extract01-tasktdesc.
*     lw_extract02-entry_id   = lw_extract01-entry_id.
        l_entry_id1 = lw_extract01-entry_id(40).
        l_entry_id2 = lw_extract01-entry_id+40(32).
        l_entry_id3 = lw_extract01-entry_id+72(1).
        lw_extract02-count      = lw_extract01-count.

        CONDENSE lw_extract02-count.
        IF l_entry_id2 IS INITIAL.
          l_entry_id2 = '_B_'.
        ENDIF.

        IF p_ushake = 'X'.
          CALL METHOD lc_ushaker->get_alias EXPORTING bname = lw_extract02-account IMPORTING alias = lw_extract02-account.
        ENDIF.

        CONCATENATE lw_extract02-tasktype   ';'  lw_extract02-account ';'  lw_extract02-tasktdesc ';'
                    l_entry_id1  ';'  l_entry_id2  ';' l_entry_id3  ';'  lw_extract02-count
                    INTO line_file01 IN CHARACTER MODE.

        TRANSFER line_file01 TO p_file.
        CLEAR: line_file01 ,lw_extract02, l_entry_id1, l_entry_id2, l_entry_id3.

      ENDLOOP.

      CLOSE DATASET p_file.
      WRITE: / 'File' ,p_file ,  ' has been generated'.
    ENDIF.
  ENDIF.
*------------------------------------------------------------------------&ROLES&USERS
  IF p_role IS NOT INITIAL.

* -USERS GENERAL DATA------------------------------------------------------

    SELECT * FROM v_username INTO TABLE lt_username.
    SELECT  bname gltgv  gltgb uflag trdat ltime  FROM usr02  INTO CORRESPONDING FIELDS OF TABLE lt_ulock.

*    IF p_ushake  = 'X'.
*      CREATE OBJECT lc_ushaker.
*    ENDIF.

    LOOP AT lt_ulock INTO lw_ulock.
      READ TABLE lt_username INTO lw_username WITH KEY bname = lw_ulock-bname.
      IF sy-subrc <> 0 OR p_ushake = 'X'.
        lw_username-persnumber = '9999999999'.
        lw_username-name_last = lw_username-name_text =
        lw_username-mc_namefir = lw_username-mc_namelas = '_B_'.
      ENDIF.

      lw_user-mandt          = sy-mandt.
      lw_user-bname          = lw_ulock-bname.
      lw_user-Persnum        = lw_username-persnumber.
      lw_user-name_last      = lw_username-name_last.
      lw_user-name_text      = lw_username-name_text.
      lw_user-mc_name_first  = lw_username-mc_namefir.
      lw_user-mc_name_last   = lw_username-mc_namelas.
      lw_user-gltgv          = lw_ulock-gltgv.
      lw_user-gltgb          = lw_ulock-gltgb.
      lw_user-trdat          = lw_ulock-trdat.
      lw_user-ltime          = lw_ulock-ltime.
      lw_user-uflag          = lw_ulock-uflag.

      IF p_ushake = 'X'.
*        lw_ushake-bname = lw_user-bname.
**       lw_user-bname   = lc_ushaker->shake( ).
        CALL METHOD lc_ushaker->get_alias EXPORTING bname = lw_user-bname IMPORTING alias = lw_user-bname.
*        lw_ushake-alias = lw_user-bname.
*        APPEND lw_ushake TO lt_ushake.
*        CLEAR lw_ushake.
      ENDIF.

      APPEND lw_user TO lt_user.

      CONCATENATE: lw_user-mandt        ';' lw_user-bname     ';' lw_user-Persnum       ';'
                   lw_user-name_last    ';' lw_user-name_text ';' lw_user-mc_name_first ';'
                   lw_user-mc_name_last ';' lw_user-gltgv     ';'  lw_user-gltgb        ';'
                   lw_user-trdat        ';' lw_user-ltime     ';' lw_user-uflag
                   INTO lw_line.
      APPEND lw_line TO lt_line.

      CLEAR: lw_username, lw_ulock.
    ENDLOOP.

    CLEAR: lt_username[], lt_ulock[].

    OPEN DATASET p_filer5 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_line INTO lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer5.
      CLEAR line_file01.
    ENDLOOP.

    CLOSE DATASET p_filer5.
    WRITE: / 'File' ,p_filer5 ,  ' has been generated'.

    CLEAR: lt_line[], lw_line.


* -TABLE AGR_USERS---------------------------------------------------
    SELECT * FROM agr_users INTO TABLE lt_agr_user.



    OPEN DATASET p_filer FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_user INTO lw_agr_user.

      IF p_ushake = 'X'.
        CALL METHOD lc_ushaker->get_alias EXPORTING bname = lw_agr_user-uname IMPORTING alias = lw_agr_user-uname.
      ENDIF.
      s_st_aux = lw_agr_user-change_tst.
      CONDENSE s_st_aux.

      CONCATENATE:  lw_agr_user-mandt      ';' lw_agr_user-agr_name   ';' lw_agr_user-uname      ';'
                    lw_agr_user-from_dat   ';' lw_agr_user-to_dat     ';' lw_agr_user-exclude    ';'
                    lw_agr_user-change_dat ';' lw_agr_user-change_tim ';' s_st_aux               ';'
                    lw_agr_user-org_flag   ';' lw_agr_user-col_flag      INTO lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      TRANSFER line_file01 TO p_filer.
      CLEAR: line_file01, lw_line,s_st_aux, lw_agr_texts.

    ENDLOOP.

    CLOSE DATASET p_filer.
    CLEAR: lt_agr_user[], lt_agr_texts[].
    WRITE: / 'File' ,p_filer ,  ' has been generated'.

* -TABLE AGR_DEFINE------------------------------------------------
    SELECT * FROM agr_define INTO TABLE lt_agr_define.
    SELECT * FROM agr_texts INTO TABLE lt_agr_texts WHERE spras = p_spra1.

    OPEN DATASET p_filer2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.


    LOOP AT lt_agr_define INTO lw_agr_define.
      READ TABLE lt_agr_texts INTO lw_agr_texts WITH KEY agr_name = lw_agr_define-agr_name.

      s_st_aux  = lw_agr_define-create_tmp.
      s_st_aux2 = lw_agr_define-change_tmp.

      CONDENSE:s_st_aux,s_st_aux2.

      CONCATENATE:  lw_agr_define-mandt      ';' lw_agr_define-agr_name   ';' lw_agr_define-parent_agr ';'
                    lw_agr_define-create_usr ';' lw_agr_define-create_dat ';' lw_agr_define-create_tim ';'
                    s_st_aux                 ';' lw_agr_define-change_usr ';' lw_agr_define-change_dat ';'
                    lw_agr_define-change_tim ';' s_st_aux2                ';' lw_agr_define-attributes ';'
                    lw_agr_texts-text INTO lw_line.

      CONDENSE lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer2.

      CLEAR: line_file01, lw_line, s_st_aux, s_st_aux2.

    ENDLOOP.

    CLOSE DATASET p_filer2.
    CLEAR lt_agr_define[].
    WRITE: / 'File' ,p_filer2 ,  ' has been generated'.

* -TABLE AGR_1251---------------------------------------------------
    SELECT * FROM agr_1251 INTO TABLE lt_agr_1251.

    OPEN DATASET p_filer3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_1251 INTO lw_agr_1251.

      CONCATENATE:  lw_agr_1251-mandt    ';' lw_agr_1251-agr_name ';' lw_agr_1251-counter  ';'
                    lw_agr_1251-object   ';' lw_agr_1251-auth     ';' lw_agr_1251-variant  ';'
                    lw_agr_1251-field    ';' lw_agr_1251-low      ';' lw_agr_1251-high     ';'
                    lw_agr_1251-modified ';' lw_agr_1251-deleted  ';' lw_agr_1251-copied   ';'
                    lw_agr_1251-neu      ';' lw_agr_1251-node     INTO lw_line.

      CONDENSE lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer3.

      CLEAR line_file01.

    ENDLOOP.

    CLOSE DATASET p_filer3.
    WRITE: / 'File' ,p_filer3 ,  ' has been generated'.

* -TABLE AGR_PROF---------------------------------------------------
    SELECT * FROM agr_PROF INTO TABLE lt_agr_PROF.

    OPEN DATASET p_filer4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_prof INTO lw_agr_prof.

      CONCATENATE:  lw_agr_prof-mandt    ';' lw_agr_prof-agr_name ';' lw_agr_prof-langu  ';'
                    lw_agr_prof-profile  ';' lw_agr_prof-ptext  INTO lw_line.

      CONDENSE lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer4.

      CLEAR: line_file01, lw_line.

    ENDLOOP.

    CLOSE DATASET p_filer4.
    WRITE: / 'File' ,p_filer4 ,  ' has been generated'.

  ENDIF.

  IF p_licens IS NOT INITIAL.

* -License Data ---------------------------------------------------

    SELECT * FROM usr06 INTO TABLE lt_usr06.
    IF sy-subrc <> 0.
      WRITE: / 'File' ,p_filer7 ,  ' has not been generated' COLOR COL_NEGATIVE .
    ELSE.
      OPEN DATASET p_filer7 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

      SELECT * FROM tutypa INTO TABLE lt_tutypa.
      SELECT * FROM tutypnow INTO TABLE lt_tutypnow.

      LOOP AT lt_usr06 INTO lw_usr06.
        CLEAR: lw_tutypa, lw_tutypnow, lw_userlic.

        READ TABLE lt_tutypa INTO lw_tUTYPA WITH KEY usertyp = lw_usr06-lic_type.
        READ TABLE lt_tutypnow INTO lw_tutypnow WITH KEY langu = p_spra1 usertyp = lw_usr06-lic_type.


        lw_userlic-usr06-mandt     = lw_usr06-mandt.
        lw_userlic-usr06-bname     = lw_usr06-bname.
        lw_userlic-usr06-lic_type  = lw_usr06-lic_type.
        lw_userlic-usr06-vondat    = lw_usr06-vondat.
        lw_userlic-usr06-bisdat    = lw_usr06-bisdat.
        lw_userlic-usr06-mandt2    = lw_usr06-mandt2.
        lw_userlic-usr06-sysid     = lw_usr06-sysid.
        lw_userlic-usr06-aname     = lw_usr06-aname.
        lw_userlic-usr06-easlpfl   = lw_usr06-easlpfl.
        lw_userlic-usr06-spras     = lw_usr06-spras.
        s_ts = lw_userlic-usr06-surcharge = lw_usr06-surcharge.
        lw_userlic-sscr_allow      = lw_tutypa-sscr_allow.
        lw_userlic-active          = lw_tutypa-active.
        lw_userlic-sondervers      = lw_tutypa-sondervers.
        lw_userlic-country         = lw_tutypa-country.
        lw_userlic-charge_info     = lw_tutypa-charge_info.
        lw_userlic-utyptext        = lw_tutypnow-utyptext.
        lw_userlic-sort            = lw_tutypnow-sort.

        IF p_ushake = 'X'.
          CALL METHOD lc_ushaker->get_alias
            EXPORTING
              bname = lw_userlic-usr06-bname
            IMPORTING
              alias = lw_userlic-usr06-bname.
        ENDIF.


        CONCATENATE: lw_userlic-usr06-mandt  ';' lw_userlic-usr06-bname   ';' lw_userlic-usr06-lic_type ';'
                     lw_userlic-usr06-vondat ';' lw_userlic-usr06-bisdat  ';' lw_userlic-usr06-mandt2 ';'
                     lw_userlic-usr06-sysid  ';' lw_userlic-usr06-aname   ';' lw_userlic-usr06-easlpfl ';'
                     lw_userlic-usr06-spras  ';' s_ts                     ';' lw_userlic-sscr_allow ';'
                     lw_userlic-active       ';' lw_userlic-sondervers    ';' lw_userlic-country ';'
                     lw_userlic-charge_info  ';' lw_userlic-utyptext      ';' lw_userlic-sort ';'
                     INTO lw_line.
        APPEND lw_line TO lt_line.

      ENDLOOP.

      LOOP AT lt_line INTO lw_line.

        CALL METHOD cl_abap_container_utilities=>fill_container_c
          EXPORTING
            im_value               = lw_line
          IMPORTING
            ex_container           = line_file01
          EXCEPTIONS
            illegal_parameter_type = 1
            OTHERS                 = 2.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        TRANSFER line_file01 TO p_filer7.

        CLEAR: line_file01, lw_line.

      ENDLOOP.

      CLOSE DATASET p_filer7.
      CLEAR: lt_line[], lw_line.
      WRITE: / 'File' ,p_filer7 ,  ' has been generated'.
    ENDIF.
  ENDIF.

  IF p_gene IS NOT INITIAL.

* -TABLE TSTCV - CSV---------------------------------------------------

    SELECT * FROM tstc  INTO TABLE lt_tstc.
    SELECT * FROM tstcv INTO TABLE lt_tstcv WHERE sprsl = p_spras.

    OPEN DATASET p_fileg1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.


    LOOP AT lt_tstc INTO lw_tstc.
      CLEAR: lw_trxs, lw_tstcv.
      READ TABLE lt_tstcv INTO lw_tstcv WITH KEY tcode = lw_tstc-tcode.
      IF sy-subrc = 0.
        lw_trxs-description = lw_tstcv-ttext.
      ELSE.
        lw_trxs-description = '_B_'.
      ENDIF.
      lw_trxs-tcode        = lw_tstc-tcode.
      lw_trxs-progname     = lw_tstc-pgmna.
      APPEND lw_trxs TO lt_trxs.

      CONCATENATE: lw_trxs-tcode ';' lw_trxs-progname ';' lw_trxs-description INTO lw_line.
      APPEND lw_line TO lt_line.

    ENDLOOP.

    LOOP AT lt_line INTO lw_line.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_line
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_fileg1.

      CLEAR: line_file01, lw_line.

    ENDLOOP.

    CLOSE DATASET p_fileg1.
    CLEAR lt_tstc[].
    WRITE: / 'File' ,p_fileg1 ,  ' has been generated'.
  ENDIF.






END-of-SELECTION.

  CLEAR: l_fileprefix.