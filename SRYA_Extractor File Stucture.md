# SRYA EXTRACTOR 
## Estructura de ficheros

> ### Versión V1.3.
> La versión 1.3.X genera todos los ficheros en fomato csv separados por punto y coma (;)

 En esta versión existen 5 grupos de ficheros:
 1. Use
 2. Roles & Users
 3. License
 4. General
 5. User Anonimization

 >No todos los ficheros han de generarse, siempre existe un flag/checkbox que hay que seleccionar previo al lanzamiento  

 ### 1. Use 
 > **Use Data File** : Fichero de uso, basado en las estadísticas de la transacción ST03 / ST03N.
 >
>>- **takstype**: Tipo de tarea (RAW). _SWNCTASKTYPERAW_. CHAR(01)
>>- **account**: User Name. _SWNCUNAME_ .CHAR(12)
>>- **Tasktdesc**: Tipo de tarea (Converted)  . _SWNCTASKTYPE_ CHAR(16)
>>- **l_entry_id1**: Trx, Report, Job a ejecutar  ._SWNCENTRYID_(40C).CHAR(40)
>>- **L_entry_id2**: Trx, Report, Job a ejecutar  ._SWNCENTRYID_+40(32C).CHAR(40) 
>>- **L_entry_id3**: Tipo de componente (T,R, )  ._SWNCENTRYID_+42(1C).CHAR(01) 
>>- **Count**: Contador de steps ._SWNCCNTAGG_. DEC(24)-CHAR(31)



### 2. Roles & Users 
Se generan cinco ficheros distintos 
 > **AGR Roles Data File** : Fichero con las asignaciones de Roles a Usuarios.
 >
>>- **mandt**: Mandante. _CLNT_. CHAR(03)
>>- **agr_name**: Nombre Role. _AGR_NAME_ .CHAR(30)
>>- **uname**: Usuario . _XUBNAME_ CHAR(12)
>>- **from_dat**: Fecha de validez inicial ._AGR_FDATE_.DATS(08)
>>- **to_dat**: Fecha de validez final  ._AGR_TDATE_.DATS(08) 
>>- **exclude**: Flag -exclusivo  ._AGR_EXCL_.CHAR(01) 
>>- **change_dat**: Fecha generación del menú ._MENU_DATE_. DATS(08) 
>>- **change_tim**: Hora de la última gen.del menú ._MENU_TIME_. TIMS(06)
>>- **change_tst**: Timestamp último cambio. _RSTIMESTMP_.DEC(15) - CHAR(19)
>>- **org_flag**: Flag asignación vien ede HR._AGR_ORG_.CHAR-(01)
>>- **col_flag**: Falg Asiganción vien de un rol compuesto._ARG_COL_.CHAR(01)

 > **AGR Define Data File** : Fichero con la definición de los roles.
 >
>>- **mandt**: Mandante. _CLNT_. CHAR(03)
>>- **agr_name**: Nombre Role. _AGR_NAME_ .CHAR(30)
>>- **parent_agr**: Nombre Role padre . _PAR_NAME_ .CHAR(30)
>>- **create_usr**: Usuario Creador . _SYUNAME_ CHAR(12)
>>- **create_dat**: Fecha generación del menú ._MENU_DATE_. DATS(08)
>>- **create_tim**: Hora de la última gen.del menú ._MENU_TIME_. TIMS(06)
>>- **create_tmp**: Timestamp creación. _RSTIMESTMP_.DEC(15)-CHAR(19) 
>>- **change_usr**: Usuario ult.modificación . _SYUNAME_ CHAR(12)
>>- **change_dat**: Fecha ult.modificación del menú ._MENU_DATE_. DATS(08)
>>- **change_tim**: Hora de la última gen.del menú ._MENU_TIME_. TIMS(06)
>>- **change_tmp**: Timestamp ult.modificación. _RSTIMESTMP_.DEC(15)-CHAR(19) 
>>- **attributes**: Attributes del ménu._MENU_ATTR_ CHAR(10)
>>- **text**: Descripción del Role _AGR_TITLE_. CHAR(80)

 > **AGR 1251 Data File** : Fichero con las autorizaciones del Role.
 >
>>- **mandt**: Mandante. _CLNT_. CHAR-03
>>- **agr_name**: Nombre Role. _AGR_NAME_ .CHAR-30
>>- **counter**: Tipo de tarea (Converted) . _MENU_NUM_6_. NUMC(6)
>>- **object**: Objeto de autorización ._AGOBJECT_.CHAR(10)
>>- **auth**: Autorización ._AGAUTH_.CHAR(12) 
>>- **variant**:Variante para el generador de perfiles ._TPR_VARI_.CHAR(04) 
>>- **field**: Nombre de campo en una autorización ._AGRFIELD_. CHAR(10)
>>- **low**: Valor menor de una autorización ._AGVAL_. CHAR(40)
>>- **high**: Valor mayor de una autorización ._AGVAL_. CHAR(40)
>>- **modified**: Status del objeto ._TPR_ST_MOD_. CHAR(1)
>>- **deleted**: Objeto borrado._TPR_ST_DEL_. CHAR(1)
>>- **copied**: Objeto copiado ._TPR_ST_COP_. CHAR(1)
>>- **neu**: Objeto nuevo ._TPR_ST_NEW_. CHAR(1)
>>- **node**: ID de nodo._SEU_ID_. NUMC(6)

 > **AGR Prof Data File** : Fichero con los nombres de perfiles por role.
 >
>>- **mandt**: Mandante. _CLNT_. CHAR(03)
>>- **agr_name**: Nombre Role. _AGR_NAME_ .CHAR(30)
>>- **langu**: Idioma (Converted)  . _LANGU_.CHAR(1)
>>- **profile**: Nombre del perfil  ._XUPROFILE_.CHAR(12)
>>- **ptext**: Descripción del perfil ._XUTEXT_.CHAR(60) 

 > **User General Data File** : Fichero con las asignaciones de Roles a Usuarios.
 >
>>- **mandt**: Mandante. _CLNT_. CHAR(03)
>>- **bname**: Usuario . _XUBNAME_.CHAR(12)
>>- **persnum**: Person Number . _PERSNUMBER_. CHAR(10)
>>- **name_last**: Last Name ._AD_NAMELAS_.CHAR(40)
>>- **name_text**: Nombre completo de persona ._AD_NAMTEXT_.CHAR(80) 
>>- **mc_name_first**: First Name en mayúsculas. _AD_MC_NMFI_. CHAR(25) 
>>- **mc_name_last**: Last name en mayúsculas._AD_MC_NMLA_.CHAR(25) 
>>- **gltgv**: Usuario válido desde._XUGLTGV_. DATS(8)
>>- **gltgb**: Usuario válido hasta._XUGLTGB_.DATS(8)
>>- **trdat**: Fecha último Logon._XULDATE_.DATS(8) 
>>- **ltime**: Hora último Logon._XULTIME_.TIMS(6) 
>>- **uflag**: Staus de bloqueo del usuario._XUUFLAG_.INT(1)-CHAR(3) 

 ### 3. License 
 > **User License Data File** : Fichero de extracción de tipología de licencias de usuario
 >
>>- **mandt**: Mandante. _CLNT_. CHAR(03)
>>- **bname**: Usuario . _XUBNAME_.CHAR(12)
>>- **lic_type**: Id tipo de licencia . _USERTYPEN_. CHAR(2)
>>- **vondat**: Sustituto desde ._XVONDAT_.DATS(8)
>>- **bisdat**: Sustituto hasta ._XBISDAT_.DATS(8) 
>>- **mandt2**: Mandante. _CLNT_. CHAR(03) 
>>- **sysid**: Nombre del sistema SAP._SYSYSID_.CHAR(8) 
>>- **aname**: Usuario sujeto a pago ._XANAME_. CHAR(12)
>>- **easlpfl**: Usuario sujeto a E-ASL ._XUTEXT_.CHAR(12)
>>- **spras**: Asignación a versión especial._VERSIONZU_.CHAR(1) 
>>- **surcharge**: Medición sistema.Recargo país._USMM_DEC3_.DEC(03)-CHAR(04) 
>>- **sscr_allow**: General Flag._FLAG_.CHAR(01) 
>>- **active**: Flag de Activo ._RSUVMFLAG_.CHAR(01) 
>>- **sondervers**: Flag de Activo ._RSUVMFLAG_.CHAR(01)  
>>- **country**: Flag de Activo ._RSUVMFLAG_.CHAR(01)  
>>- **charge_info**: Med. sistema:info pre. ususario._SLIM_CHARGE_INFO_LITYPE_.CHAR(01) 
>>- **utyptext**: Texto para tipos de usuario ._UTYPTEXT_.CHAR(30) 
>>- **sort**: Clasificación tipo de usuario ._RSUVMSORT_.NUMC(06) 

 ### 4. General 
 > **Transaction Data File** : 
 >
>>- **tcode**: Código de transacción. _TCODE_. CHAR(20)
>>- **progname**: Nombre de Progname . _PROGRAM_ID_.CHAR(40)
>>- **description**: Descripción de transacción . _TTEXT_STCT_. CHAR(36)

 ### 5. User Anonimization
 > **Alias Data File** : 
 >
>>- **bname**: Código de transacción. _TCODE_. CHAR(20)
>>- **alias**: Nombre de Progname . _PROGRAM_ID_.CHAR(40)
>>- **description**: Descripción de transacción . _TTEXT_STCT_. CHAR(36)