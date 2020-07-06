**&--------------------------------------------------------------------*
*& Z_IDR_SRYA_EXTRACTOR
*&---------------------------------------------------------------------*
*&                        LICENSE CONDITIONS                           *
*                                                                      *
*  Z_IDR_SRYA_EXTRACTOR : IDR Scan Roles y Autorizaciones              *
*                                                                      *
*  Copyright (C) 2020 Novis Euforia                                    *
*                                                                      *
*  This program is free software: you can redistribute it and/or modify*
*  it under the terms of the GNU Affero General Public License as      *
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
*  2020.04.03 : SRYA EXTRACTOR V1.1 : Miscellaneous
*  2020.05.19 : SRYA EXTRACTOR V1.2 : TRXS extractrion
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
        bname         TYPE xubname,
        Persnum       TYPE ad_persnum,
        name_last     TYPE ad_namelas,
        name_text     TYPE ad_namtext,
        mc_name_first TYPE ad_mc_nmfi,
        mc_name_last  TYPE ad_mc_nmla,
        trdat         TYPE xuldate,
        ltime         TYPE xultime,
        uflag         TYPE string,
      END OF t_user.


TYPES: BEGIN OF t_ulock,
         bname TYPE xubname,
         trdat TYPE xuldate,
         ltime TYPE xultime,
         uflag TYPE xuuflag,
       END OF t_ulock.

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

CLASS lcl_winpath IMPLEMENTATION.
  METHOD constructor.

    me->l_file = i_file.

  ENDMETHOD.                    "constructor

  METHOD get_winpath.

    me->lo_winfs =  cl_fs_windows_path=>create_windows_path( name = me->l_file ).
    l_winpath = lo_winfs->get_path_name( ).

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
      lw_agr_1251   TYPE agr_1251,
      lw_agr_prof   TYPE agr_prof,
      lw_usr01      TYPE usr01,
      lt_agr_user   TYPE STANDARD TABLE OF agr_users,
      lt_agr_define TYPE STANDARD TABLE OF agr_define,
      lt_agr_1251   TYPE STANDARD TABLE OF agr_1251,
      lt_agr_prof   TYPE STANDARD TABLE OF agr_prof,
      lt_username   TYPE STANDARD TABLE OF v_username,
      lw_username   TYPE v_username,
      lt_user       TYPE STANDARD TABLE OF t_user,
      lw_user       TYPE t_user,
      lt_ulock      TYPE STANDARD TABLE OF t_ulock,
      lw_ulock      TYPE t_ulock,
      lt_tstcv      TYPE STANDARD TABLE OF tstcv,
      lw_tstcv      TYPE tstcv,
      lt_tstc       TYPE STANDARD TABLE OF tstc,
      lw_tstc       TYPE tstc,
      lt_trxs       TYPE STANDARD TABLE OF t_trxs,
      lw_trxs       TYPE t_trxs.

DATA: line_file01 TYPE string,
      pname(80).

DATA: ts   TYPE timestamp,
      s_ts TYPE string.

DATA: lw_opsystem TYPE opsystem,
      l_lifesWin  TYPE filesys_d VALUE 'WINDOWS NT',
      lc_winpath  TYPE REF TO lcl_winpath,
      lw_file     TYPE t_file,
      lt_files    TYPE STANDARD TABLE OF t_file.

DATA : lw_line TYPE string,
       lt_line TYPE STANDARD TABLE OF string.

*-$-SELECTION SCREEN
SELECTION-SCREEN BEGIN OF BLOCK use WITH FRAME TITLE title0.
  PARAMETERS: p_use    AS CHECKBOX DEFAULT 'X',
              p_comp   TYPE swnchostname DEFAULT 'TOTAL',
              p_periot TYPE swncperitype DEFAULT 'M',
              p_perios TYPE swncdatum DEFAULT sy-datum,
              p_summar TYPE swnc_cflag DEFAULT '',
              p_file   TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt'.
SELECTION-SCREEN END OF BLOCK use.
SELECTION-SCREEN BEGIN OF BLOCK rol WITH FRAME TITLE title1.
  PARAMETERS: p_role   AS   CHECKBOX DEFAULT '',
              p_filer  TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt',
              p_filer2 TYPE rlgrap-filename  DEFAULT '/tmp/test02.txt',
              p_filer3 TYPE rlgrap-filename  DEFAULT '/tmp/test03.txt',
              p_filer4 TYPE rlgrap-filename  DEFAULT '/tmp/test04.txt',
              p_filer5 TYPE rlgrap-filename  DEFAULT '/tmp/test05.txt'.
SELECTION-SCREEN END OF BLOCK rol.

SELECTION-SCREEN BEGIN OF BLOCK gen WITH FRAME TITLE title2.
  PARAMETERS: p_gene   AS CHECKBOX DEFAULT '',
              p_spras  TYPE spras DEFAULT 'E' NO-DISPLAY,
              p_fileg1 TYPE rlgrap-filename  DEFAULT '/tmp/test06.txt'.
SELECTION-SCREEN END OF BLOCK gen.

*SELECTION-SCREEN ULINE /1(50).
SELECTION-SCREEN COMMENT /10(50) l_instr.
*SELECTION-SCREEN ULINE /1(50).


AT SELECTION-SCREEN.


*-$- SELSCR
INITIALIZATION.

  title0 = 'Use'.
  title1 = 'Roles & Users'.
  title2 = 'General'.
  GET TIME STAMP FIELD ts.
  s_ts = ts.
  CONDENSE s_ts.

* CLEAR p_file.
*  CONCATENATE:  '/tmp/Use_' p_comp '_' p_periot '_' p_perios'.txt' INTO p_file.

  CONCATENATE   '/tmp/Use_' p_comp '_' p_periot '_' p_perios'.txt' INTO p_file.
  lw_file-file = p_file. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Rol_AGR_USERS_'  s_ts '.txt' INTO p_filer.
  lw_file-file = p_filer. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Rol_AGR_DEFINE_' s_ts '.txt' INTO p_filer2.
  lw_file-file = p_filer2. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Rol_AGR_1251_'   s_ts '.txt' INTO p_filer3.
  lw_file-file = p_filer3. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Rol_AGR_PROF_'   s_ts '.txt' INTO p_filer4.
  lw_file-file = p_filer4. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Rol_USR01_'      s_ts '.txt' INTO p_filer5.
  lw_file-file = p_filer5. APPEND lw_file TO lt_files.
  CONCATENATE   '/tmp/Gen_TSTC_'       s_ts '.txt' INTO p_fileg1.
  lw_file-file = p_fileg1. APPEND lw_file TO lt_files.

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
          p_file = lc_winpath->get_winpath( ).
        WHEN 2.
          p_filer = lc_winpath->get_winpath( ).
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
      ENDCASE.

      FREE lc_winpath.
    ENDLOOP.

  ENDIF.

  l_instr = 'Copyright © 2020 Novis Euforia'.

START-OF-SELECTION.

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
      lw_extract02-entry_id   = lw_extract01-entry_id.
      lw_extract02-count      = lw_extract01-count.


      CONCATENATE lw_extract02-tasktype ';'  lw_extract02-account ';'  lw_extract02-tasktdesc ';'
                  lw_extract02-entry_id   ';'  lw_extract02-count
              INTO line_file01 IN CHARACTER MODE.

      TRANSFER line_file01 TO p_file.
      CLEAR: line_file01 ,lw_extract02.

    ENDLOOP.

    CLOSE DATASET p_file.
    WRITE: 'Fichero' ,p_file ,  ' copiado correctamente'.
  ENDIF.
*-----------------------------------------------------------------------
  IF p_role IS NOT INITIAL.

* -TABLE USR01------------------------------------------------------
    SELECT * FROM v_username INTO TABLE lt_username.
    SELECT  bname uflag trdat ltime  FROM usr02  INTO CORRESPONDING FIELDS OF TABLE lt_ulock.

    LOOP AT lt_ulock INTO lw_ulock.
      READ TABLE lt_username INTO lw_username WITH KEY bname = lw_ulock-bname.
      IF sy-subrc <> 0.
        lw_username-persnumber = '9999999999'.
        lw_username-name_last = lw_username-name_text =
        lw_username-mc_namefir = lw_username-mc_namelas = 'TBD'.
      ENDIF.

      lw_user-bname          = lw_ulock-bname.
      lw_user-Persnum        = lw_username-persnumber.
      lw_user-name_last      = lw_username-name_last.
      lw_user-name_text      = lw_username-name_text.
      lw_user-mc_name_first  = lw_username-mc_namefir.
      lw_user-mc_name_last   = lw_username-mc_namelas.
      lw_user-trdat          = lw_ulock-trdat.
      lw_user-ltime          = lw_ulock-ltime.
      lw_user-uflag          = lw_ulock-uflag.

      APPEND lw_user TO lt_user.

      CONCATENATE: lw_user-bname ';' lw_user-Persnum ';' lw_user-name_last
                   lw_user-name_text ';' lw_user-mc_name_first ';' lw_user-mc_name_last
                   lw_user-trdat ';' lw_user-ltime ';' lw_user-uflag INTO lw_line.
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
    WRITE: / 'Fichero' ,p_filer5 ,  ' copiado correctamente'.

  ENDIF.
  CLEAR: lt_line[], lw_line.

* -TABLE AGR_USERS---------------------------------------------------
    SELECT * FROM agr_users INTO TABLE lt_agr_user.

    OPEN DATASET p_filer FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_user INTO lw_agr_user.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_agr_user
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      TRANSFER line_file01 TO p_filer.

      CLEAR line_file01.

    ENDLOOP.

    CLOSE DATASET p_filer.
    CLEAR lt_agr_user[].
    WRITE: / 'Fichero' ,p_filer ,  ' copiado correctamente'.

* -TABLE AGR_DEFINE------------------------------------------------
    SELECT * FROM agr_DEFINE INTO TABLE lt_agr_define.

    OPEN DATASET p_filer2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_define INTO lw_agr_define.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_agr_define
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer2.

      CLEAR line_file01.

    ENDLOOP.

    CLOSE DATASET p_filer2.
    CLEAR lt_agr_define[].
    WRITE: / 'Fichero' ,p_filer2 ,  ' copiado correctamente'.

* -TABLE AGR_1251---------------------------------------------------
    SELECT * FROM agr_1251 INTO TABLE lt_agr_1251.

    OPEN DATASET p_filer3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_1251 INTO lw_agr_1251.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_agr_1251
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
    WRITE: / 'Fichero' ,p_filer3 ,  ' copiado correctamente'.

* -TABLE AGR_PROF---------------------------------------------------
    SELECT * FROM agr_PROF INTO TABLE lt_agr_PROF.

    OPEN DATASET p_filer4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_agr_prof INTO lw_agr_prof.

      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = lw_agr_prof
        IMPORTING
          ex_container           = line_file01
        EXCEPTIONS
          illegal_parameter_type = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      TRANSFER line_file01 TO p_filer4.

      CLEAR line_file01.

    ENDLOOP.

    CLOSE DATASET p_filer4.
    WRITE: / 'Fichero' ,p_filer4 ,  ' copiado correctamente'.


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
        lw_trxs-description = 'TBD'.
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

      CLEAR line_file01.

    ENDLOOP.

    CLOSE DATASET p_fileg1.
    CLEAR lt_tstc[].
    WRITE: / 'Fichero' ,p_fileg1 ,  ' copiado correctamente'.
  ENDIF.