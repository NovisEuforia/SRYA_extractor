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
*  2021.05.31 : SRYA EXTRACTOR V1.9 : Use data Collect
*  2021.06.02 : SRYA EXTRACTOR V2.0 : New Data extraction -USO*-
*  2021.07.12 : SRYA EXTRACTOR V2.1a: Windows-based paths issue Fix
************************************************************************


*-Types
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
         devclass    TYPE devclass,
       END OF t_trxs.
TYPES:BEGIN OF t_user,
        mandt         TYPE symandt,
        bname         TYPE xubname,
        persnum       TYPE ad_persnum,
        name_last     TYPE ad_namelas,
        name_text     TYPE ad_namtext,
        mc_name_first TYPE ad_mc_nmfi,
        mc_name_last  TYPE ad_mc_nmla,
        gltgv         TYPE xugltgv,
        gltgb         TYPE xugltgb,
        trdat         TYPE xuldate,
        ltime         TYPE xultime,
        uflag         TYPE string,
        addrnumber    TYPE ad_addrnum,
        idadtype      TYPE numc2,
        partner       TYPE bu_partner,
        bptype        TYPE bu_type,
        bpkind        TYPE bu_bpkind,
        bpgroup       TYPE bu_group,
        organization  TYPE bu_partner,
        orgname       TYPE bu_sort1,
        country       TYPE land1,
        developer     TYPE flag,
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

*-Local Classes
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
  ENDMETHOD.
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

*-Data
DATA: lt_ucode     TYPE STANDARD TABLE OF swncaggusertcode,
      lt_ucode_par TYPE STANDARD TABLE OF swncaggusertcode,
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
      lt_usr21      TYPE STANDARD TABLE OF usr21,
      lw_usr21      TYPE usr21,
      lt_addr       TYPE STANDARD TABLE OF v_addr_usr,
      lw_addr       TYPE v_addr_usr,
      lt_dev        TYPE STANDARD TABLE OF devaccess,
      lw_dev        TYPE devaccess,
      lw_but000     TYPE but000,
      lw_but000a    TYPE but000,
      lt_ust04      TYPE STANDARD TABLE OF ust04,
      lw_ust04      TYPE ust04,
      lt_ushake     TYPE STANDARD TABLE OF t_ushake,
      lw_ushake     TYPE t_ushake,
      lt_tstcv      TYPE STANDARD TABLE OF tstcv,
      lw_tstcv      TYPE tstcv,
      lt_tstc       TYPE STANDARD TABLE OF tstc,
      lw_tstc       TYPE tstc,
      lt_tdevc      TYPE STANDARD TABLE OF tDEVC,
      lw_tdevc      TYPE tdevc,
      lt_tutyp      TYPE STANDARD TABLE OF tutyp,
      lw_tutyp      TYPE tutyp,
      lt_info_tran  TYPE STANDARD TABLE OF info_tran,
      lw_info_tran  TYPE info_tran,
      lt_trxs       TYPE STANDARD TABLE OF t_trxs,
      lw_trxs       TYPE t_trxs,
      lw_userlic    TYPE t_userlic,
      lw_usr06      TYPE usr06,
      lt_usr06      TYPE STANDARD TABLE OF usr06,
      lw_tutypa     TYPE tutypa,
      lt_tutypa     TYPE STANDARD TABLE OF tutypa,
      lw_tutypnow   TYPE tutypnow,
      lt_tutypnow   TYPE STANDARD TABLE OF tutypnow,
      lw_usobt      TYPE usobt,
      lt_usobt      TYPE STANDARD TABLE OF usobt,
      lw_usobx      TYPE usobx,
      lt_usobx      TYPE STANDARD TABLE OF usobx,
      lw_usobt_c    TYPE usobt_c,
      lt_usobt_c    TYPE STANDARD TABLE OF usobt_c,
      lw_usobx_c    TYPE usobx_c,
      lt_usobx_c    TYPE STANDARD TABLE OF usobx_c,
      lw_agr_flags  TYPE agr_flags,
      lt_agr_flags  TYPE  STANDARD TABLE OF agr_flags,
      lw_agr_hier   TYPE agr_hier,
      lt_agr_hier   TYPE STANDARD TABLE OF agr_hier,
      lw_df14vd     TYPE df14vd,
      lt_df14vd     TYPE STANDARD TABLE OF df14vd.
DATA: line_file01   TYPE string,
      pname(80),
      l_perios_last TYPE swncdatum.
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
FIELD-SYMBOLS: <fs_fileline> TYPE t_file.
DATA: lc_ushaker           TYPE REF TO lcl_ushaker,
      l_fileprefix         TYPE string,
      l_pathprefix         TYPE string,
      l_filename           TYPE string,
      l_prevpath           TYPE string,
      l_newpath            TYPE string,
      l_sysclient          TYPE string,
      l_nameuse            TYPE string,
      l_namerolagruser     TYPE string,
      l_namerolagrdefine   TYPE string,
      l_namerolagr1251     TYPE string,
      l_namerolagrflags    TYPE string,
      l_namerolagrhier     TYPE string,
      l_namerolagrauthprof TYPE string,
      l_namerolagrprofile  TYPE string,
      l_namerolagrusob     TYPE string,
      l_nameusersmaster    TYPE string,
      l_nametrxsmaster     TYPE string,
      l_nameappcomponent   TYPE string,
      l_namedevclass       TYPE string,
      l_nameusertypes      TYPE string,
      l_nameuseralias      TYPE string,
      l_nameuserlic        TYPE string,
      l_prev_custo         TYPE text12.
DATA: l_pathprefixlength    TYPE i,
      l_len_prev_custo      TYPE i,
      l_len_prev_custo_keep TYPE i,
      l_errmsg              TYPE string.
DATA: convin  TYPE REF TO cl_abap_conv_in_ce,
      lcx_err TYPE REF TO cx_root.

*-Selection Screen

SELECTION-SCREEN BEGIN OF BLOCK cli WITH FRAME TITLE title4.
  PARAMETERS: p_custo TYPE text12 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK cli.


SELECTION-SCREEN BEGIN OF BLOCK use WITH FRAME TITLE title0.
  PARAMETERS: p_use    AS CHECKBOX DEFAULT 'X',
              p_comp   TYPE swnchostname DEFAULT 'TOTAL' NO-DISPLAY,
*              p_periot TYPE swncperitype DEFAULT 'M',
              p_perios TYPE swncdatum DEFAULT sy-datum,
              p_summar TYPE swnc_cflag DEFAULT '' NO-DISPLAY,
              p_file   TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt'.
SELECTION-SCREEN END OF BLOCK use.
SELECTION-SCREEN BEGIN OF BLOCK rol WITH FRAME TITLE title1.
  PARAMETERS: p_role   AS   CHECKBOX DEFAULT 'X',
              p_filer  TYPE rlgrap-filename  DEFAULT '/tmp/test01.txt',
              p_filer2 TYPE rlgrap-filename  DEFAULT '/tmp/test02.txt',
              p_spra1  TYPE spras DEFAULT sy-langu NO-DISPLAY,
              p_filer3 TYPE rlgrap-filename  DEFAULT '/tmp/test03.txt',
              p_file3a TYPE rlgrap-filename  DEFAULT '/tmp/test03a.txt',
              p_file3b TYPE rlgrap-filename  DEFAULT '/tmp/test03b.txt',
              p_file3c TYPE rlgrap-filename  DEFAULT '/tmp/test03c.txt',
              p_filer4 TYPE rlgrap-filename  DEFAULT '/tmp/test04.txt',
              p_file4a TYPE rlgrap-filename  DEFAULT '/tmp/test04a.txt',
              p_filer5 TYPE rlgrap-filename  DEFAULT '/tmp/test05.txt'.
SELECTION-SCREEN END OF BLOCK rol.
SELECTION-SCREEN BEGIN OF BLOCK lic WITH FRAME TITLE title5.
  PARAMETERS: p_licens TYPE flag DEFAULT 'X',
              p_filer7 TYPE rlgrap-filename  DEFAULT '/tmp/license.txt'.
SELECTION-SCREEN END OF BLOCK lic.
SELECTION-SCREEN BEGIN OF BLOCK gen WITH FRAME TITLE title2.
  PARAMETERS: p_gene   AS CHECKBOX DEFAULT 'X',
              p_spras  TYPE spras DEFAULT 'E' NO-DISPLAY,
              p_fileg1 TYPE rlgrap-filename  DEFAULT '/tmp/test06.txt',
              p_fileg2 TYPE rlgrap-filename  DEFAULT '/tmp/test06b.txt',
              p_fileg3 TYPE rlgrap-filename  DEFAULT '/tmp/test06c.txt',
              p_fileg4 TYPE rlgrap-filename  DEFAULT '/tmp/test06c.txt'.
SELECTION-SCREEN END OF BLOCK gen.
SELECTION-SCREEN BEGIN OF BLOCK uan WITH FRAME TITLE title3.
  PARAMETERS: p_ushake TYPE flag DEFAULT '',
              p_filer6 TYPE rlgrap-filename  DEFAULT '/tmp/alias.txt'.
SELECTION-SCREEN END OF BLOCK uan.
SELECTION-SCREEN COMMENT /10(50) l_instr.
*---------------------------------------------------------------------
AT SELECTION-SCREEN.
*---------------------------------------------------------------------
  CONCATENATE p_custo '_' sy-sysid sy-mandt INTO l_fileprefix.
  l_len_prev_custo_keep = l_len_prev_custo.
  LOOP AT lt_files ASSIGNING <fs_fileline>.
    l_pathprefixlength = strlen( l_pathprefix ).
    l_prevpath = <fs_fileline>-file(l_pathprefixlength).
    l_len_prev_custo = l_len_prev_custo_keep.

    CASE sy-tabix.
      WHEN 1.
        IF p_custo = l_prev_custo.
          SEARCH p_file FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_file FOR l_sysclient.
          ELSE.
            SEARCH p_file FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_file(sy-fdpos) p_custo p_file+l_len_prev_custo INTO p_file.
            <fs_fileline>-file = p_file.
          ENDIF.
        ENDIF.
        SEARCH p_file FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_file FOR l_fileprefix.
        ELSE.
          SEARCH p_file FOR l_sysclient.
        ENDIF.
        l_newpath = p_file(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nameuse INTO <fs_fileline>-file.
        p_file =  <fs_fileline>-file.

      WHEN 2.
        IF p_custo = l_prev_custo.
          SEARCH p_filer FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer FOR l_sysclient.
          ELSE.
            SEARCH p_filer FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer(sy-fdpos) p_custo p_filer+l_len_prev_custo INTO p_filer.
            <fs_fileline>-file = p_filer.
          ENDIF.
        ENDIF.
        SEARCH p_filer FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer FOR l_fileprefix.
        ELSE.
          SEARCH p_filer FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagruser INTO <fs_fileline>-file.
        p_filer = <fs_fileline>-file.
      WHEN 3.
        IF p_custo = l_prev_custo.
          SEARCH p_filer2 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer2 FOR l_sysclient.
          ELSE.
            SEARCH p_filer2 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer2(sy-fdpos) p_custo p_filer2+l_len_prev_custo INTO p_filer2.
            <fs_fileline>-file = p_filer2.
          ENDIF.
        ENDIF.
        SEARCH p_filer2 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer2 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer2 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer2(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrdefine INTO <fs_fileline>-file.
        p_filer2 = <fs_fileline>-file.
      WHEN 4.
        IF p_custo = l_prev_custo.
          SEARCH p_filer3 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer3 FOR l_sysclient.
          ELSE.
            SEARCH p_filer3 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer3(sy-fdpos) p_custo p_filer3+l_len_prev_custo INTO p_filer3.
            <fs_fileline>-file = p_filer3.
          ENDIF.
        ENDIF.
        SEARCH p_filer3 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer3 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer3 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer3(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagr1251 INTO <fs_fileline>-file.
        p_filer3 = <fs_fileline>-file.
      WHEN 5.
        IF p_custo = l_prev_custo.
          SEARCH p_file3a FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_file3a FOR l_sysclient.
          ELSE.
            SEARCH p_file3a FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_file3a(sy-fdpos) p_custo p_file3a+l_len_prev_custo INTO p_file3a.
            <fs_fileline>-file = p_file3a.
          ENDIF.
        ENDIF.
        SEARCH p_file3a FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_file3a FOR l_fileprefix.
        ELSE.
          SEARCH p_file3a FOR l_sysclient.
        ENDIF.
        l_newpath = p_file3a(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrflags INTO <fs_fileline>-file.
        p_file3a = <fs_fileline>-file.
      WHEN 6.
        IF p_custo = l_prev_custo.
          SEARCH p_file3b FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_file3b FOR l_sysclient.
          ELSE.
            SEARCH p_file3b FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_file3b(sy-fdpos) p_custo p_file3b+l_len_prev_custo INTO p_file3b.
            <fs_fileline>-file = p_file3b.
          ENDIF.
        ENDIF.
        SEARCH p_file3b FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_file3b FOR l_fileprefix.
        ELSE.
          SEARCH p_file3b FOR l_sysclient.
        ENDIF.
        l_newpath = p_file3b(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrhier INTO <fs_fileline>-file.
        p_file3b = <fs_fileline>-file.
      WHEN 7.
        IF p_custo = l_prev_custo.
          SEARCH p_file3c FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_file3c FOR l_sysclient.
          ELSE.
            SEARCH p_file3c FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_file3c(sy-fdpos) p_custo p_file3c+l_len_prev_custo INTO p_file3c.
            <fs_fileline>-file = p_file3c.
          ENDIF.
        ENDIF.
        SEARCH p_file3c FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_file3c FOR l_fileprefix.
        ELSE.
          SEARCH p_file3c FOR l_sysclient.
        ENDIF.
        l_newpath = p_file3c(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrauthprof INTO <fs_fileline>-file.
        p_file3c = <fs_fileline>-file.
      WHEN 8.
        IF p_custo = l_prev_custo.
          SEARCH p_filer4 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer4 FOR l_sysclient.
          ELSE.
            SEARCH p_filer4 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer4(sy-fdpos) p_custo p_filer4+l_len_prev_custo INTO p_filer4.
            <fs_fileline>-file = p_filer4.
          ENDIF.
        ENDIF.
        SEARCH p_filer4 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer4 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer4 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer4(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrprofile INTO <fs_fileline>-file.
        p_filer4 = <fs_fileline>-file.
      WHEN 9.
        IF p_custo = l_prev_custo.
          SEARCH p_file4a FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_file4a FOR l_sysclient.
          ELSE.
            SEARCH p_file4a FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_file4a(sy-fdpos) p_custo p_file4a+l_len_prev_custo INTO p_file4a.
            <fs_fileline>-file = p_file4a.
          ENDIF.
        ENDIF.
        SEARCH p_file4a FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_file4a FOR l_fileprefix.
        ELSE.
          SEARCH p_file4a FOR l_sysclient.
        ENDIF.
        l_newpath = p_file4a(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namerolagrusob INTO <fs_fileline>-file.
        p_file4a = <fs_fileline>-file.
      WHEN 10.
        IF p_custo = l_prev_custo.
          SEARCH p_filer5 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer5 FOR l_sysclient.
          ELSE.
            SEARCH p_filer5 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer5(sy-fdpos) p_custo p_filer5+l_len_prev_custo INTO p_filer5.
            <fs_fileline>-file = p_filer5.
          ENDIF.
        ENDIF.
        SEARCH p_filer5 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer5 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer5 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer5(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nameusersmaster INTO <fs_fileline>-file.
        p_filer5 = <fs_fileline>-file.
      WHEN 11.
        IF p_custo = l_prev_custo.
          SEARCH p_fileg1 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_fileg1 FOR l_sysclient.
          ELSE.
            SEARCH p_fileg1 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_fileg1(sy-fdpos) p_custo p_fileg1+l_len_prev_custo INTO p_fileg1.
            <fs_fileline>-file = p_fileg1.
          ENDIF.
        ENDIF.
        SEARCH p_fileg1 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_fileg1 FOR l_fileprefix.
        ELSE.
          SEARCH p_fileg1 FOR l_sysclient.
        ENDIF.
        l_newpath = p_fileg1(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nametrxsmaster INTO <fs_fileline>-file.
        p_fileg1 = <fs_fileline>-file.
      WHEN 12.
        IF p_custo = l_prev_custo.
          SEARCH p_fileg2 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_fileg2 FOR l_sysclient.
          ELSE.
            SEARCH p_fileg2 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_fileg2(sy-fdpos) p_custo p_fileg2+l_len_prev_custo INTO p_fileg2.
            <fs_fileline>-file = p_fileg2.
          ENDIF.
        ENDIF.
        SEARCH p_fileg2 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_fileg2 FOR l_fileprefix.
        ELSE.
          SEARCH p_fileg2 FOR l_sysclient.
        ENDIF.
        l_newpath = p_fileg2(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix  l_nameappcomponent INTO <fs_fileline>-file.
        p_fileg2 = <fs_fileline>-file.
      WHEN 13.
        IF p_custo = l_prev_custo.
          SEARCH p_fileg3 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_fileg3 FOR l_sysclient.
          ELSE.
            SEARCH p_fileg3 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_fileg3(sy-fdpos) p_custo p_fileg3+l_len_prev_custo INTO p_fileg3.
            <fs_fileline>-file = p_fileg3.
          ENDIF.
        ENDIF.
        SEARCH p_fileg3 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_fileg3 FOR l_fileprefix.
        ELSE.
          SEARCH p_fileg3 FOR l_sysclient.
        ENDIF.
        l_newpath = p_fileg3(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_namedevclass INTO <fs_fileline>-file.
        p_fileg3 = <fs_fileline>-file.
      WHEN 14.
        IF p_custo = l_prev_custo.
          SEARCH p_fileg4 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_fileg4 FOR l_sysclient.
          ELSE.
            SEARCH p_fileg4 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_fileg4(sy-fdpos) p_custo p_fileg4+l_len_prev_custo INTO p_fileg4.
            <fs_fileline>-file = p_fileg4.
          ENDIF.
        ENDIF.
        SEARCH p_fileg4 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_fileg4 FOR l_fileprefix.
        ELSE.
          SEARCH p_fileg4 FOR l_sysclient.
        ENDIF.
        l_newpath = p_fileg4(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nameusertypes INTO <fs_fileline>-file.
        p_fileg4 = <fs_fileline>-file.
      WHEN 15.
        IF p_custo = l_prev_custo.
          SEARCH p_filer6 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer6 FOR l_sysclient.
          ELSE.
            SEARCH p_filer6 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer6(sy-fdpos) p_custo p_filer6+l_len_prev_custo INTO p_filer6.
            <fs_fileline>-file = p_filer6.
          ENDIF.
        ENDIF.
        SEARCH p_filer6 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer6 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer6 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer6(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nameuseralias INTO <fs_fileline>-file.
        p_filer6 = <fs_fileline>-file.
      WHEN 16.
        IF p_custo = l_prev_custo.
          SEARCH p_filer7 FOR p_custo.
        ELSE.
          IF l_prev_custo IS INITIAL.
            SEARCH p_filer7 FOR l_sysclient.
          ELSE.
            SEARCH p_filer7 FOR l_prev_custo.
            l_len_prev_custo = l_len_prev_custo + sy-fdpos.
            CONCATENATE p_filer7(sy-fdpos) p_custo p_filer7+l_len_prev_custo INTO p_filer7.
            <fs_fileline>-file = p_filer7.
          ENDIF.
        ENDIF.
        SEARCH p_filer7 FOR p_custo.
        IF sy-subrc = 0.
          SEARCH p_filer7 FOR l_fileprefix.
        ELSE.
          SEARCH p_filer7 FOR l_sysclient.
        ENDIF.
        l_newpath = p_filer7(sy-fdpos).
        l_pathprefix = l_newpath.
        CONCATENATE l_newpath l_fileprefix l_nameuserlic INTO <fs_fileline>-file.
        p_filer7 = <fs_fileline>-file.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  l_prev_custo = p_custo.
  l_len_prev_custo = strlen( p_custo ).

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
          p_file3a = lc_winpath->get_winpath( ).
        WHEN 6.
          p_file3b = lc_winpath->get_winpath( ).
        WHEN 7.
          p_file3c = lc_winpath->get_winpath( ).
        WHEN 8.
          p_filer4 = lc_winpath->get_winpath( ).
        WHEN 9.
          p_file4a = lc_winpath->get_winpath( ).
        WHEN 10.
          p_filer5 = lc_winpath->get_winpath( ).
        WHEN 11.
          p_fileg1 = lc_winpath->get_winpath( ).
        WHEN 12.
          p_fileg2 = lc_winpath->get_winpath( ).
        WHEN 13.
          p_fileg3 = lc_winpath->get_winpath( ).
        WHEN 14.
          p_fileg4 = lc_winpath->get_winpath( ).
        WHEN 15.
          p_filer6 = lc_winpath->get_winpath( ).
        WHEN 16.
          p_filer7 = lc_winpath->get_winpath( ).
      ENDCASE.
      FREE lc_winpath.
    ENDLOOP.
  ENDIF.
*---------------------------------------------------------------------
INITIALIZATION.
*---------------------------------------------------------------------
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = '00'
      months    = '03'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = p_perios.
  p_perios+6(2) = '01'.

  %_P_COMP_%_app_%-text   = 'Component'.
  %_P_CUSTO_%_app_%-text  = 'Customer Name'.
  %_P_FILE_%_app_%-text   = 'Use Data File'.
  %_P_FILEG1_%_app_%-text = 'Transaction Data File'.
  %_P_FILEG2_%_app_%-text = 'App Component Data File'.
  %_P_FILEG3_%_app_%-text = 'Dev Class Data File'.
  %_P_FILEG4_%_app_%-text = 'User types Data File'.
  %_P_FILER_%_app_%-text  = 'AGR User-Roles Data File'.
  %_P_FILER2_%_app_%-text = 'AGR Define Data File'.
  %_P_FILER3_%_app_%-text = 'AGR 1251 Data File'.
  %_P_FILE3A_%_app_%-text = 'AGR Flags Data File'.
  %_P_FILE3B_%_app_%-text = 'AGR Hier Data File'.
  %_P_FILE3C_%_app_%-text = 'AGR Auth.Profile Data File'.
  %_P_FILER4_%_app_%-text = 'AGR Prof Data File'.
  %_P_FILE4A_%_app_%-text = 'AGR USOB Data File'.
  %_P_FILER5_%_app_%-text = 'USER General Data File'.
  %_P_FILER6_%_app_%-text = 'Alias Data File'.
  %_P_GENE_%_app_%-text   = 'General Data Generation'.
  %_P_PERIOS_%_app_%-text = 'Initial Date'.
*%_P_PERIOT_%_app_%-text = 'Period'.
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

  CONCATENATE sy-sysid sy-mandt INTO l_sysclient.
  l_fileprefix = l_sysclient.
  l_pathprefix = '/tmp/'.
  CONCATENATE '_USE_'        p_comp '_' p_perios'.csv' INTO l_nameuse.
  CONCATENATE '_ROLAGR_USER'        '_' s_ts '.csv'    INTO l_namerolagruser.
  CONCATENATE '_ROLAGR_DEFINE'      '_' s_ts '.csv'    INTO l_namerolagrdefine.
  CONCATENATE '_ROLAGR_1251'        '_' s_ts '.csv'    INTO l_namerolagr1251.
  CONCATENATE '_ROLAGR_FLAGS'       '_' s_ts '.csv'    INTO l_namerolagrflags.
  CONCATENATE '_ROLAGR_HIER'        '_' s_ts '.csv'    INTO l_namerolagrhier.
  CONCATENATE '_ROLAGR_AUTHPROF'    '_' s_ts '.csv'    INTO l_namerolagrauthprof.
  CONCATENATE '_ROLAGR_PROFILE'     '_' s_ts '.csv'    INTO l_namerolagrprofile.
  CONCATENATE '_ROLAGR_USOB'        '_' s_ts '.csv'    INTO l_namerolagrusob.
  CONCATENATE '_USERS_MASTER'       '_' s_ts '.csv'    INTO l_nameusersmaster.
  CONCATENATE '_TRXS_MASTER'        '_' s_ts '.csv'    INTO l_nametrxsmaster.
  CONCATENATE '_APP_COMPONENT'      '_' s_ts '.csv'    INTO l_nameappcomponent.
  CONCATENATE '_DEVCLASS'           '_' s_ts '.csv'    INTO l_namedevclass.
  CONCATENATE '_USER_TYPES'         '_' s_ts '.csv'    INTO l_nameusertypes.
  CONCATENATE '_USERS_ALIAS'        '_' s_ts '.csv'    INTO l_nameuseralias.
  CONCATENATE '_USERS_LIC'          '_' s_ts '.csv'    INTO l_nameuserlic.

  CONCATENATE    l_pathprefix l_fileprefix l_nameuse            INTO p_file.
  lw_file-file = p_file.   APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagruser     INTO p_filer.
  lw_file-file = p_filer.  APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix  l_namerolagrdefine  INTO p_filer2.
  lw_file-file = p_filer2. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagr1251     INTO p_filer3.
  lw_file-file = p_filer3. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagrflags    INTO p_file3a.
  lw_file-file = p_file3a. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagrhier     INTO p_file3b.
  lw_file-file = p_file3b. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagrauthprof INTO p_file3c.
  lw_file-file = p_file3b. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagrprofile  INTO p_filer4.
  lw_file-file = p_filer4. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namerolagrusob     INTO p_file4a.
  lw_file-file = p_file4a. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nameusersmaster    INTO p_filer5.
  lw_file-file = p_filer5. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nametrxsmaster     INTO p_fileg1.
  lw_file-file = p_fileg1. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nameappcomponent   INTO p_fileg2.
  lw_file-file = p_fileg2. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_namedevclass       INTO p_fileg3.
  lw_file-file = p_fileg3. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nameusertypes      INTO p_fileg4.
  lw_file-file = p_fileg4. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nameuseralias      INTO p_filer6.
  lw_file-file = p_filer6. APPEND lw_file TO lt_files.
  CONCATENATE    l_pathprefix l_fileprefix l_nameuserlic        INTO p_filer7.
  lw_file-file = p_filer7. APPEND lw_file TO lt_files.

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
          p_file3a = lc_winpath->get_winpath( ).
        WHEN 6.
          p_file3b = lc_winpath->get_winpath( ).
        WHEN 7.
          p_file3c = lc_winpath->get_winpath( ).
        WHEN 8.
          p_filer4 = lc_winpath->get_winpath( ).
        WHEN 9.
          p_file4a = lc_winpath->get_winpath( ).
        WHEN 10.
          p_filer5 = lc_winpath->get_winpath( ).
        WHEN 11.
          p_fileg1 = lc_winpath->get_winpath( ).
        WHEN 12.
          p_fileg2 = lc_winpath->get_winpath( ).
        WHEN 13.
          p_fileg3 = lc_winpath->get_winpath( ).
        WHEN 14.
          p_fileg4 = lc_winpath->get_winpath( ).
        WHEN 15.
          p_filer6 = lc_winpath->get_winpath( ).
        WHEN 16.
          p_filer7 = lc_winpath->get_winpath( ).
      ENDCASE.
      FREE lc_winpath.
    ENDLOOP.
  ENDIF.
  l_instr = 'Copyright © 2020 Novis Euforia'.
*---------------------------------------------------------------------
START-OF-SELECTION.
*---------------------------------------------------------------------
  IF p_ushake  = 'X'.
    CREATE OBJECT lc_ushaker.
  ENDIF.
  IF p_ushake = 'X'.

    OPEN DATASET p_filer6 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer6 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
  IF p_use IS NOT INITIAL.
    l_perios_last = p_perios.
    l_perios_last+6(2) = '01'.
    DO.
      CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
        EXPORTING
          component     = p_comp
          periodtype    = 'M'
          periodstrt    = l_perios_last
          summary_only  = p_summar
*         STORAGE_TYPE  = ' '
        TABLES
          usertcode     = lt_ucode_par
        EXCEPTIONS
          no_data_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        "MESSAGE 'No data selected'(001) TYPE 'E'.
      ELSE.
        IF lt_ucode IS INITIAL.
          lt_ucode = lt_ucode_par.
        ELSE.
          APPEND LINES OF lt_ucode_par TO lt_ucode.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = l_perios_last
          days      = '00'
          months    = '01'
          signum    = '+'
          years     = '00'
        IMPORTING
          calc_date = l_perios_last.
      IF l_perios_last > sy-datum.
        EXIT.
      ENDIF.
    ENDDO.
*-$-
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
      IF sy-subrc NE 0.
        CONCATENATE 'Error al abrir el fichero ' p_file INTO l_errmsg RESPECTING BLANKS.
        MESSAGE l_errmsg TYPE 'E'.
      ENDIF.

      LOOP AT lt_extract01 INTO lw_extract01.
        lw_extract02-tasktype   = lw_extract01-tasktype.
        lw_extract02-account    = lw_extract01-account.
        lw_extract02-tasktdesc  = lw_extract01-tasktdesc.
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
        CONCATENATE lw_extract02-tasktype ';' lw_extract02-account ';'  lw_extract02-tasktdesc ';'
                    l_entry_id1 ';' l_entry_id2  ';' l_entry_id3 ';'  lw_extract02-count
                    INTO line_file01 IN CHARACTER MODE.
        TRANSFER line_file01 TO p_file.
        CLEAR: line_file01 ,lw_extract02, l_entry_id1, l_entry_id2, l_entry_id3.
      ENDLOOP.
      CLOSE DATASET p_file.
      WRITE: / 'File' ,p_file ,  ' has been generated'.
    ENDIF.
  ENDIF.
  IF p_role IS NOT INITIAL.
    SELECT * FROM v_username INTO TABLE lt_username.
    SELECT  bname gltgv  gltgb uflag trdat ltime  FROM usr02  INTO CORRESPONDING FIELDS OF TABLE lt_ulock.
    SELECT * FROM usr21 INTO TABLE lt_usr21.
    SELECT * FROM v_addr_usr INTO TABLE lt_addr.
    SELECT * FROM devaccess INTO TABLE lt_dev.
    LOOP AT lt_ulock INTO lw_ulock.
      READ TABLE lt_username INTO lw_username WITH KEY bname = lw_ulock-bname.
      IF sy-subrc <> 0 OR p_ushake = 'X'.
        lw_username-persnumber = '9999999999'.
        lw_username-name_last = lw_username-name_text =
        lw_username-mc_namefir = lw_username-mc_namelas = '_B_'.
      ENDIF.
      READ TABLE lt_usr21 INTO lw_usr21 WITH KEY bname = lw_ulock-bname.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM but000 INTO lw_but000 WHERE partner_guid = lw_usr21-bpperson.
        SELECT SINGLE * FROM but000 INTO lw_but000a WHERE partner_guid = lw_usr21-organization.
        READ TABLE lt_addr INTO lw_addr WITH KEY addrnumber = lw_usr21-addrnumber persnumber = lw_usr21-persnumber.
        IF lw_addr-country IS NOT INITIAL.
          lw_user-country = lw_addr-country.
        ELSE.
          lw_user-country = lw_addr-namcountry.
        ENDIF.
      ENDIF.
      READ TABLE lt_dev INTO lw_dev WITH KEY uname = lw_ulock-bname.
      IF sy-subrc = 0.
        lw_user-developer = 'X'.
      ENDIF.
      lw_user-mandt         = sy-mandt.
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
      lw_user-addrnumber     = lw_usr21-addrnumber.
      lw_user-idadtype       = lw_usr21-idadtype .
      lw_user-partner        = lw_but000-partner.
      lw_user-bptype         = lw_but000-type.
      lw_user-bpkind         = lw_but000-bpkind.
      lw_user-bpgroup        = lw_but000-bu_group.
      lw_user-organization   = lw_but000a-partner.
      lw_user-orgname        = lw_but000a-bu_sort1.
      IF p_ushake = 'X'.
        CALL METHOD lc_ushaker->get_alias EXPORTING bname = lw_user-bname IMPORTING alias = lw_user-bname.
      ENDIF.
      APPEND lw_user TO lt_user.
      CONCATENATE: lw_user-mandt        ';' lw_user-bname     ';' lw_user-Persnum       ';'
                   lw_user-name_last    ';' lw_user-name_text ';' lw_user-mc_name_first ';'
                   lw_user-mc_name_last ';' lw_user-gltgv     ';' lw_user-gltgb        ';'
                   lw_user-trdat        ';' lw_user-ltime     ';' lw_user-uflag         ';'
                   lw_user-addrnumber   ';' lw_user-idadtype  ';' lw_user-partner       ';'
                   lw_user-bptype       ';' lw_user-bpkind    ';' lw_user-bpgroup       ';'
                   lw_user-organization ';' lw_user-orgname   ';' lw_user-country       ';'
                   lw_user-developer
                   INTO lw_line.
      APPEND lw_line TO lt_line.
      CLEAR: lw_username, lw_ulock ,lw_usr21, lw_but000, lw_but000a,lw_addr, lw_dev.
    ENDLOOP.
    CLEAR: lt_username[], lt_ulock[] ,lt_usr21[], lt_addr[], lt_dev[].

    OPEN DATASET p_filer5 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer5 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
    SELECT * FROM agr_users INTO TABLE lt_agr_user.

    OPEN DATASET p_filer FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
    SELECT * FROM agr_define INTO TABLE lt_agr_define.
    SELECT * FROM agr_texts INTO TABLE lt_agr_texts WHERE spras = p_spra1.

    OPEN DATASET p_filer2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer2 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
    SELECT * FROM agr_1251 INTO TABLE lt_agr_1251.

    OPEN DATASET p_filer3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer3 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
    SELECT * FROM agr_flags INTO TABLE lt_agr_flags.

    OPEN DATASET p_file3a FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_file3a INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_agr_flags INTO lw_agr_flags.
      CONCATENATE:  lw_agr_flags-mandt ';' lw_agr_flags-agr_name ';'
                    lw_agr_flags-flag_type ';' lw_agr_flags-flag_value
                    INTO lw_line.
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
      TRANSFER line_file01 TO p_file3a.
      CLEAR line_file01.
    ENDLOOP.
    CLOSE DATASET p_file3a.
    WRITE: / 'File' ,p_file3a ,  ' has been generated'.
    SELECT * FROM agr_hier INTO TABLE lt_agr_hier.

    OPEN DATASET p_file3b FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_file3b INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_agr_hier INTO lw_agr_hier.
      CONCATENATE:  lw_agr_hier-mandt      ';' lw_agr_hier-agr_name ';' lw_agr_hier-object_id  ';'
                    lw_agr_hier-parent_id  ';' lw_agr_hier-folder   ';' lw_agr_hier-attributes  ';'
                    lw_agr_hier-source_agr ';' lw_agr_hier-flags    ';' lw_agr_hier-appl_alias
                    INTO lw_line.
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
      TRANSFER line_file01 TO p_file3b.
      CLEAR line_file01.
    ENDLOOP.
    CLOSE DATASET p_file3b.
    WRITE: / 'File' ,p_file3b ,  ' has been generated'.
    SELECT * FROM agr_PROF INTO TABLE lt_agr_PROF.

    OPEN DATASET p_filer4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_filer4 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
    SELECT * FROM ust04 INTO TABLE lt_ust04.

    OPEN DATASET p_file3c FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_file3c INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_ust04 INTO lw_ust04.
      CONCATENATE:  lw_ust04-mandt    ';' lw_ust04-bname ';' lw_ust04-profile
              INTO lw_line.
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
      TRANSFER line_file01 TO p_file3c.
      CLEAR: line_file01, lw_line.
    ENDLOOP.
    CLOSE DATASET p_file3c.
    WRITE: / 'File' ,p_file3c ,  ' has been generated'.
    SELECT * FROM usobt INTO TABLE lt_usobt WHERE type = 'TR'.
    LOOP AT lt_usobt INTO lw_usobt.
      CONCATENATE 'U01' ';' lw_usobt-name ';'  lw_usobt-type ';' lw_usobt-object ';'
                  lw_usobt-field ';' lw_usobt-low ';' lw_usobt-high INTO lw_line.
      CONDENSE lw_line.
      APPEND lw_line TO lt_line.
      CLEAR lw_line.
    ENDLOOP.
    SELECT * FROM usobx INTO TABLE lt_usobx WHERE type = 'TR'.
    LOOP AT lt_usobx INTO lw_usobx.
      CONCATENATE 'U02' ';' lw_usobx-name ';'  lw_usobx-type ';' lw_usobx-object ';'
                  lw_usobx-okflag INTO lw_line.
      CONDENSE lw_line.
      APPEND lw_line TO lt_line.
      CLEAR lw_line.
    ENDLOOP.
    SELECT * FROM usobt_c INTO TABLE lt_usobt_c WHERE type = 'TR'.
    LOOP AT lt_usobt_c INTO lw_usobt_c.
      CONCATENATE 'U03' ';' lw_usobt_c-name ';'  lw_usobt_c-type ';' lw_usobt_c-object ';'
                  lw_usobt_c-field ';' lw_usobt_c-low ';' lw_usobt_c-high INTO lw_line.
      CONDENSE lw_line.
      APPEND lw_line TO lt_line.
      CLEAR lw_line.
    ENDLOOP.
    SELECT * FROM usobx_c INTO TABLE lt_usobx_c WHERE type = 'TR'.
    LOOP AT lt_usobx_c INTO lw_usobx_c.
      CONCATENATE 'U04' ';' lw_usobx_c-name';'  lw_usobx_c-type ';' lw_usobx_c-object ';'
                  lw_usobx_c-okflag ';' lw_usobx_c-orgname  INTO lw_line.
      CONDENSE lw_line.
      APPEND lw_line TO lt_line.
      CLEAR lw_line.
    ENDLOOP.

    OPEN DATASET p_file4a FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_file4a INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

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
      TRANSFER line_file01 TO p_file4a.
      CLEAR: line_file01, lw_line.
    ENDLOOP.
    CLOSE DATASET p_file4a.
    CLEAR: lt_line[], lw_line.
    WRITE: / 'File' ,p_file4a ,  ' has been generated'.
  ENDIF.
  IF p_licens IS NOT INITIAL.
    SELECT * FROM usr06 INTO TABLE lt_usr06.
    IF sy-subrc <> 0.
      WRITE: / 'File' ,p_filer7 ,  ' has not been generated' COLOR COL_NEGATIVE .
    ELSE.
      OPEN DATASET p_filer7 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc NE 0.
        CONCATENATE 'Error al abrir el fichero ' p_filer7 INTO l_errmsg RESPECTING BLANKS.
        MESSAGE l_errmsg TYPE 'E'.
      ENDIF.

      SELECT * FROM tutypa INTO TABLE lt_tutypa.
      SELECT * FROM tutypnow INTO TABLE lt_tutypnow.
      LOOP AT lt_usr06 INTO lw_usr06.
        CLEAR: lw_tutypa, lw_tutypnow, lw_userlic.
        READ TABLE lt_tutypa INTO lw_tUTYPA WITH KEY usertyp = lw_usr06-lic_type.
        READ TABLE lt_tutypnow INTO lw_tutypnow WITH KEY langu = p_spra1 usertyp = lw_usr06-lic_type.
        lw_userlic-usr06-mandt         = lw_usr06-mandt.
        lw_userlic-usr06-bname         = lw_usr06-bname.
        lw_userlic-usr06-lic_type      = lw_usr06-lic_type.
        lw_userlic-usr06-vondat        = lw_usr06-vondat.
        lw_userlic-usr06-bisdat        = lw_usr06-bisdat.
        lw_userlic-usr06-mandt2        = lw_usr06-mandt2.
        lw_userlic-usr06-sysid         = lw_usr06-sysid.
        lw_userlic-usr06-aname         = lw_usr06-aname.
        lw_userlic-usr06-easlpfl       = lw_usr06-easlpfl.
        lw_userlic-usr06-spras            = lw_usr06-spras.
        s_ts = lw_userlic-usr06-surcharge = lw_usr06-surcharge.
        lw_userlic-sscr_allow          = lw_tutypa-sscr_allow.
        lw_userlic-active              = lw_tutypa-active.
        lw_userlic-sondervers          = lw_tutypa-sondervers.
        lw_userlic-country             = lw_tutypa-country.
        lw_userlic-charge_info         = lw_tutypa-charge_info.
        lw_userlic-utyptext            = lw_tutypnow-utyptext.
        lw_userlic-sort                = lw_tutypnow-sort.
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
        CONDENSE lw_line.
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
    SELECT * FROM info_tran  INTO TABLE lt_info_tran.
    SELECT * FROM tstcv INTO TABLE lt_tstcv WHERE sprsl = p_spras.

    OPEN DATASET p_fileg1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_fileg1 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_info_tran INTO lw_info_tran.
      CLEAR: lw_trxs, lw_tstcv.
      READ TABLE lt_tstcv INTO lw_tstcv WITH KEY tcode = lw_info_tran-tcode.
      IF sy-subrc = 0.
        lw_trxs-description = lw_tstcv-ttext.
      ELSE.
        lw_trxs-description = '_B_'.
      ENDIF.
      lw_trxs-tcode        = lw_info_tran-tcode.
      lw_trxs-progname     = lw_info_tran-pgmna.
      lw_trxs-devclass     = lw_info_tran-devclass.
      APPEND lw_trxs TO lt_trxs.
      CONCATENATE: lw_trxs-tcode ';' lw_trxs-progname ';' lw_trxs-description
                   ';' lw_trxs-devclass INTO lw_line.
      CONDENSE lw_line.
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
    CLEAR: lt_tstc[],lt_info_tran[].
    CLEAR: lt_line[], lw_line.
    WRITE: / 'File' ,p_fileg1 ,  ' has been generated'.
    SELECT * FROM df14vd  INTO TABLE lt_df14vd WHERE langu = sy-langu.

    OPEN DATASET p_fileg2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_fileg2 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_df14vd INTO lw_df14vd.
      CONCATENATE: lw_df14vd-fctr_id     ';' lw_df14vd-as4local ';'
                   lw_df14vd-name        ';' lw_df14vd-rele ';'
                   lw_df14vd-lstrele     ';' lw_df14vd-ps_posid ';'
                   lw_df14vd-xref        ';' lw_df14vd-released ';'
                   lw_df14vd-incomplete  INTO lw_line.
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
      TRANSFER line_file01 TO p_fileg2.
      CLEAR: line_file01, lw_line.
    ENDLOOP.
    CLOSE DATASET p_fileg2.
    CLEAR lt_tstc[].
    WRITE: / 'File' ,p_fileg2 ,  ' has been generated'.
    SELECT * FROM tdevc INTO TABLE lt_tdevc.

    OPEN DATASET p_fileg3 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_fileg3 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_tdevc INTO lw_tdevc.
      CONCATENATE: lw_tdevc-devclass  ';' lw_tdevc-dlvunit ';'
                   lw_tdevc-component ';' lw_tdevc-namespace ';'
                   lw_tdevc-shipment  ';' lw_tdevc-parentcl ';'
                   lw_tdevc-applicat   INTO lw_line.
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
      TRANSFER line_file01 TO p_fileg3.
      CLEAR: line_file01, lw_line.
    ENDLOOP.
    CLOSE DATASET p_fileg3.
    CLEAR lt_tdevc[].
    WRITE: / 'File' ,p_fileg3 ,  ' has been generated'.
    SELECT * FROM tutyp INTO TABLE lt_tutyp WHERE langu = sy-langu.

    OPEN DATASET p_fileg4 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONCATENATE 'Error al abrir el fichero ' p_fileg4 INTO l_errmsg RESPECTING BLANKS.
      MESSAGE l_errmsg TYPE 'E'.
    ENDIF.

    LOOP AT lt_tutyp INTO lw_tutyp.
      CONCATENATE: lw_tutyp-usertyp  ';' lw_tutyp-utyptext ';'
                   lw_tutyp-utyplongtext INTO lw_line.
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
      TRANSFER line_file01 TO p_fileg4.
      CLEAR: line_file01, lw_line.
    ENDLOOP.
    CLOSE DATASET p_fileg4.
    CLEAR lt_tutyp[].
    WRITE: / 'File' ,p_fileg4 ,  ' has been generated'.
  ENDIF.
*---------------------------------------------------------------------
END-of-SELECTION.
*---------------------------------------------------------------------
  CLEAR: l_fileprefix.
