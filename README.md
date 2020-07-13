# SRYA_extractor
SRYA_extractor : IDR Roles y autorizaciones


**Extractor para el análisis de roles y Segregación funciones**. Además de extraer el uso del sistema por parte de los usuarios, extrae información de los roles del sistema, su asignación a los usuarios e información general de los usuarios.  

El extractor esta formado por un solo report **-Z_IDR_SRYA_EXTRACTOR-** autocontenido - no necesita ningún componente _no estándar_ adicional. 

## Información extraída en la versión actual 1.3
La versión actual del extractor obtiene la siguiente información del sistema:

1. **Uso**: Uso del sistema por parte de los usuarios.
2. **Usuarios y Roles** : Información de Roles, Autorizaciones y su asignación a usuarios. 
3. **Transacciones**: Información de las transacciones del sistema.
4. **Licencias**: Información sobre el tipo de licencias de los usuarios.


## Método de despliegue del report en el sistema
Según cualquiera de las siguientes opciones: 

1. Creación del report **de forma manual:**

   1. Creación del report Z_IDR_SRYA_EXTRACTOR mediante la transacción SE38 o SE80
   2. Copiar el código fuente del fichero -REPS Z_IDR_SRYA_EXTRACTOR.abap- ubicado en ![/objects/PROG/Z_IDR_SRYA_EXTRACTOR](https://github.com/NovisEuforia/SRYA_extractor/blob/master/objects/PROG/Z_IDR_SRYA_EXTRACTOR/REPS%20Z_IDR_SRYA_EXTRACTOR.abap)
   3. Grabar y report , meterlo en una Orden de transporte
   4. Desplegarlo en el entorno en donde se realizar la extracción
  
2. Mediante Importación de **orden de transporte:**

   1. Se descarga la última orden de transporte de la ubicación  ![/SRYA_extractor/ChangeRequest/](https://github.com/NovisEuforia/SRYA_extractor/blob/master/ChangeRequest).
   2. Se importa la orden en el _TMS - Transport Management System_ - _Transacción STMS_
   3. Se despliega la orden el entorno en donde se va a realizar la extracción
   
3. Mediante Clonado/Copia del repositorio **GitHub:** _(Solo S4 y abapGIT)_

   1. Sistemas **S4** con gCTS: Clonado del repositorio **SRYA_extractor** en landscape destino y despliegue en el entorno donde se va a realizar la extracción.
   2. Sistemas **no S4 con ![abapGIT](https://github.com/larshp/abapGit.git)**: Clonado del repositorio **SRYA_extractor** en landscape destino y despliegue en el entorno donde se va a realizar la extracción.  
   
## Ejecución del Scan

Los requisitos para poder ejecutar el report son :

1. Estar loggados en el sistema de la extracción en **idioma inglés** 
2. El usuario que le ejecute debe tener el perfil estándar **S_TOOL_EX**
  
   
  El report tiene 6 partes diferenciadas:  
**1.Customer**: información del cliente.

**2.Uso**: información de uso del sistema.

**3.Roles & Ususarios**: información de los Roles y Usuarios.

**4.licencias**:información sobre el tipo de licencias de los usuarios.

**5.General**: información general.

**6.Anonimización**: Información sobre los usuarios anonimizados.

  Debido a lo heterogeneo y ocasionalmente voluminoso de la información a extraer, el report deposita la información extraída en un directorio del servidor de aplicación permitiendo así la ejecución del report en modo backgorund.
  
  El report se ejecuta directamente desde la **SE38**:
![Pantalla de selección](https://github.com/NovisEuforia/SRYA_extractor/blob/master/files/Z_IDR_SRYA_EXTRACTOR_DYNP1000.png?raw=true)  
    
 **Pantalla de Selección**
 
 **1.Customer**:Información del cliente, solo tiene el parámetro obligatorio _Customer name_, este parámetro sirve para generar el nombre de los ficheros y poder ser identificados de forma automática en el sistema.
 
 **2.Uso**: Información de los datos de uso, tiene los siguientes parámetros:
 
* **_Use Data generation_**   : _Grabar fichero de uso_ = "X".
* **_Period_** : _Periodo de análisis_ En principio los análisis siempre serán mensuales = "M".
* **_Date_**   : _Fecha de análisis (Cualquier del mes a analizar)_  = Por defecto se rellena con el primer día del mes anterior a la fecha del sistema.
* **_Use Data File_** : _Nombre del fichero_ Generado automáticamente.            

El primer parámetro indica que se quiere generar la información de uso, después se índica la fecha, la recomendación es generar la información entre **_los 3 y 6 últimos meses de uso_**. La mejor opción es seleccioinar el primer día de cada mes. E.g: si se desea generar la información de los tres últimos meses y hoy es _09.07.2020_  habrá que hacer tres lanzamientos con las fechas _01.06.2020, 01.05.2020 y 01.04.2020 -. 

El nombre de todos los ficheros se genera automáticamente, se recomienda no modificar el nombre de los ficheros salvo el directorio que por defecto es el **_tmp_**.

**3.Roles & Usuarios**: información sobre los Roles del sistema, su jerarquía y autorizaciones. Por otro lado extrae información de los usuarios y la asignaciones de Roles y Usuarios.

* **_R & U Data Generation_** : _Grabar ficheros datos R & U_ = "X".
* **_AGR Roles Data File_** : _Nombre del fichero_ Generado automáticamente. _Asignación de Roles a usuarios_
* **_AGR Define Data File_** : _Nombre del fichero_ Generado automáticamente. _Definición de Roles_ 
* **_AGR 1251 Data File_** : _Nombre del fichero_ Generado automáticamente. _Autorizaciones_
* **_AGR Prof Data File_** : _Nombre del fichero_ Generado automáticamente. _Perfiles_
* **_USER General Data File_** : _Nombre del fichero_ Generado automáticamente. _Datos generales usuario_

Al marcar el parámetro **_R & U Data Generation_** - se generan cinco ficheros que son volcados en el _file system_ del servidor. El nombre de los ficheros es autogenerado. Se recomienda no modificar ningún nombre de fichero salvo que se quiera cambiar el directorio de grabación.

En general la ejecución de está sección no debería tener problema de lanzarse Online.

**4.Licencias**: información sobre el tipo de licencia que tienen los usuarios del sistema.

* **_License Data Generation_** : _Grabar fichero de Datos de Licencia_ = "X".
* **_User License Data File_** : _Nombre del fichero_ Generado automáticamente. _Tipo de licencia por usuario_

Al marcar el parámetro **_License Data Generation_** - se genera un fichero que es volcado en el _file system_ del servidor. El nombre del fichero es autogenerado. Se recomienda no modificar ningún nombre de fichero salvo que se quiera cambiar el directorio de grabación.

**5.General**: información general.

* **_General Data Generation_** : _Grabar fichero de Datos Generales_ = "X".
* **_Transaction Data File_** : _Nombre del fichero_ Generado automáticamente. _Transacciones del sistema_

Al marcar el parámetro **_General Data Generation_**  - se genera un fichero que es volcado en el _file system_ del servidor. El nombre del fichero es autogenerado. Se recomienda no modificar ningún nombre de fichero salvo que se quiera cambiar el directorio de grabación.

**6.User Anonimization**: Anonimización de los datos de usuario.

* **_User Alias Generation_** : _Grabar fichero de Alias_ = "X".
* **_Alias Data File_** : _Nombre del fichero_ Generado automáticamente. _Matriz Código Usuario - Alias_

Al marcar el parámetro **_User Alias Generation_**  - se genera un fichero que es volcado en el _file system_ del servidor. El nombre del fichero es autogenerado. Se recomienda no modificar ningún nombre de fichero salvo que se quiera cambiar el directorio de grabación.

Al marcar el parámetro **_User Alias Generation_**  además se anonimizan todos los ficheros donde aparezca el nombre del usuario, la forma de realizar esta anonimización es sustituyendo el código de usario por un alias autogenerado en todos los ficheros donde aparece el código de usuario: (**_Use Data File , AGR Roles Data File y USER General Data File_**). El fichero _Alias Data File_ tiene el _matching_ del Código de usuario y el Alias. Este fichero no debe enviarse, se lo debe quedar el cliente para posteriormente poder identificar los usuarios en base a los Alias que son los únicos que apareceran en el informe. De esta forma Novis Euforia solo tiene acceso a información de los alias.

## Enviío de los resultados del Scan

Una vez terminado el scan con todos los ficheros hay que descargarlos del servidor _**transacción CG3Y**_ , zippearlos y enviarlos según el modo que se haya indicado.
