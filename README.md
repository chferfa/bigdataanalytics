# bigdataanalytics
## Text Mining en Social Media
### Máster Big Data Analytics - Curso 2016 / 2017
### Universitat Politècnica de València

#### Guión de ejecución

<u>_Paper_</u>

El _paper_ se ha realizado mediante _LaTeX_.

Se aporta tanto el documento _PDF_ con el resultado final (**paper.pdf**), como el fichero con el código fuente _LaTeX_ (**paper.tex**); además, para la correcta compilación, se aportan las imágenes referenciadas en el código fuente _LaTeX_, que deben ubicarse en el mismo directorio que el fichero con el código fuente.

<u>_Scripts_</u>

Los _scripts_ se han realizado mediante _R_.

Se aportan dos _scripts_:

**my-pan-ap17.R**

Este _script_ incluye la tarea completa. Dentro del _script_ se encuentra cada apartado convenientemente comentado y separado.

El _script_ se divide en 3 apartados:

Preparación

- Se instalan las librerías (el script comprueba primero si ya están instaladas)
- Se cargan las librerías
- <u>Se configuran las rutas del _dataset_ (ruta de _training_ y ruta de _test_)</u>
- Se establecen los parámetros de configuración del preprocesado de los _datasets_ tanto a nivel global como a nivel particular (para cada uno de los dos problemas)
- Se definen las funciones auxiliares

Detección de género

- Obtención del vocabulario
- Obtención de las bolsas de palabras
- Obtención del modelo de _machine learning_ mediante **Random Forest**

Detección de variedad

- Obtención del vocabulario
- Obtención de las bolsas de palabras
- Obtención del modelo de _machine learning_ mediante **Random Forest**


**exploration.R**

Este _script_ es independiente del anterior y complementario a la tarea. En él se incluyen las acciones realizadas sobre los _datasets_ para llevar a cabo la exploración de los mismos.
Parte de los resultados de la ejecución de este _script_ está plasmada en el _paper_.

Si ya se dispone de todas las librerías (y, en caso, contrario, si no surge ningún inconveniente en la instalación de las mismas) <u>la única configuración indispensable para ejecutar el ambos _scripts_ es establecer las rutas del _dataset_</u>.
