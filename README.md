# SRYA_extractor
SRYA_extractor


**Extractor para el análisis de roles y Segregación funciones**. Además de extraer el uso del sistema por parte de los usuarios, extrae información de los roles del sistema y su asignación a los usuarios. 

El extractor es solo un report **-Z_IDR_SRYA_EXTRACTOR-** autocontenido - no necesita ningún componente _no estándar_ adicional. 

## Información extraída en la versión actual 1.2
La versión actual del extractor obtiene la siguiente información del sistema:

1. Uso : Actualmente se extrae de la transaccción _ST03/ST03N_
2. Usuarios y Roles : Se extrae la información de diversas tablas _AGR* y USR*_
3. Transacciones : Se extrae información de vistas estándar del sistema.  

## Método de importación del report
Según cualquiera de las siguientes opciones: 

1. Creación del report **de forma manual:**

   1. Creación del report Z_IDR_SRYA_EXTRACTOR mediante la transacción SE38 o SE80
   2. Copiar el código fuente del fichero -REPS Z_IDR_SRYA_EXTRACTOR.abap-
   3. Grabar y report , meterlo en una Orden de transporte
   4. Desplegarlo en el entorno en donde se realizar la extracción
  
2. Mediante Importación de **orden de transporte:**

   1. Se descarga el fichero XXX XXX ubicado en : hyperlink
   2. Se importa la orden en _TMS - Transport Management System_ - _Transacción STMS_
   3. Se despliega la orden el entorno en donde se va a realizar la extracción
   
3. Mediante Clonado/Copia del repositorio **GitHub:** _(Solo S4 y abapGIT)-

   1. Sistemas **S4** con gCTS: Clonado del repositorio **SRYA_extractor** en landscape destino y despliegue en el entorno donde se va a realizar la extracción.
   2. Sistemas **no S4 con [abapGIT](https://github.com/larshp/abapGit.git)**: Clonado del repositorio **SRYA_extractor** en landscape destino y despliegue en el entorno donde se va a realizar la extracción.  
   
## Ejecución del Scan

Los requiistos para poder ejecutar el report son :

1. Estar loggados en inglés 
2. El usuario que le ejecute debe tener el perfil estándar **S_TOOL_EX**
  
Se recomienda el lanzamiento en modo batch ya que debido al volumen puede sobrepasar el timeout de diálogo. 
  
  El report tiene 3 partes difenreciadas:  **USO, Roles y Transacciones**. Debido a lo heterogeneo y voluminoso de la información y a extraer, el report deposita  la información extraída en un directorio del servidor de aplicación.
  
  El report se ejecuta directamente desde la **SE38**:
![Pantalla de selección](https://raw.githubusercontent.com/NovisEuforia/SRYA_extractor/master/files/Z_IDR_SRYA_EXTRACTOR_DYNP1000.png?token=AOMOIWWKO32ZTFPJ4RZDEJK62QZFQ)  
  
  
La captura de la izquierda muestra la pantalla de selección del report sin importar los elementos de texto y a la derecha como sería con los elementos de texto importados ![Fichero textos](https://github.com/NovisEuforia/SRYA_extractor/blob/master/objects/PROG/Z_IDR_SRYA_EXTRACTOR/REPT%20Z_IDR_SRYA_EXTRACTOR.asx.xml).

 Extracción de **datos de uso:** 

Lo normal será extraer los datos de uso del último mes completo, para ello se ha de seleccionar las siguientes opciones:

* P_USE                        = "X"                        - _(Marcado) - Grabar fichero de uso_
* P_COMP   (Component)         = "TOTAL"                    - _Componentes analizados (Todos)_
* P_PERIOS (Date)              = Fecha del análisis         - _Fecha de lectura de la información (Cualquier del mes a analizar)_ 
* P_PERIOT (Period Type)       = "M"                        - _Extraer información del Mes_
* P_SUMMR (Summary Only)       = " "                        - _Extraer información summarizada_
* P_FILE                       = Se genera automáticamente  - _Nombre del fichero generado_            


ELa ejecución depositará un fichero en el servidor de aplicación ( se puede acceder por la **transacción AL11**), en el _directorio TMP_ con el nombre autogenerado _"<Use_TOTAL_M_<fecha>.txt>"_.

 Extracción de **Roles y usuarios**. 

Marcar el parámetro "**P_ROLE"** - el report genera 5 ficheros que son volcados en el servidor. De igual forma el nombre de los ficheros es autogenerado con la regla **"Rol_<Nombre de tabla a volcar>_timestamp.txt"**. En principio no es necesario modificar ningún nombre de fichero salvo que se quiera cambiar el directorio de grabación.

La ejecución de los roles no debe tener problema de lanzarse Online.

 Extracción de **Transacciones**. 

Marcar el parámetro **"P_GENE"** -y se generará un fichero con los códigos de transacciones del sistema.

Una vez terminado el scan hay que descargarlos del servidor _**transacción CG3Y**_ , zippearlos y enviarlos según el criterio que se haya indicado.
