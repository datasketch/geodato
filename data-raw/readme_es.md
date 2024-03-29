# Estructura carpeta geodato

Dentro de la carpeta geodato, se debe crear una carpeta con el código ISO del país al que pertenece la división geográfica que se desea agregar en el paquete. A continuación, dentro de esa carpeta, se crea una subcarpeta cuyo nombre debe seguir el siguiente formato:

**`<carpeta_principal>_<iso_entidad>_<nombre_división_administrativa>`**.

Por ejemplo, si se quiere agregar el mapa de una ciudad de México, se agrega la carpeta con el ISO del país, el sufijo de esa ciudad y la división geográfica correspondiente al topojson que desea guardar. Supongamos que es la Ciudad de México (CDMX) y la división geográfica que se va a guardar es "alcaldías" (dejando el término en inglés como "mayors"). Entonces, el nombre de la carpeta sería mex_cdmx_mayors.

Esta carpeta debe contener los siguientes archivos:

-   El archivo TopoJSON del mapa.
-   Los centroides de las regiones. (ver sección: centroides_file)
-   Un archivo meta.yaml que contenga la información necesaria sobre la división geográfica.

Opcionalmente puede guardar otras tablas con nombres alternativos y regiones. La estructura de estos datos será definida durante el desarrollo del documento.

## Configuración archivo meta.yaml

El archivo meta.yaml contiene información general y descriptiva de las propiedades del mapa que desea adjuntar. A continuación se explican las distintas secciones que debe contener:

### label

Es la etiqueta o el nombre descriptivo del mapa

### centroids_file

Indica la ruta que contiene una base de datos que relaciona los identificadores y el nombre de las divisiones geográficas del mapa con sus centros.

La estructura del archivo csv debe ser la siguiente:

| id  | name     | lat   | lon    |
|-----|----------|-------|--------|
| 1   | nombre 1 | -1.92 | -71.98 |
| 2   | nombre 2 | 7.14  | -75.58 |
| 3   | nombre 3 | 6.28  | -70.73 |
| ... | ...      | ...   | ...    |

Las columnas id y name incluyen los códigos y nombres de cada división geográfica que deben coincidir con el sistema estadístico nacional. Las columnas lat y lon incluyen las coordenadas geográficas (latitud y longitud) del centroide de cada región.

### altnames_file (opcional)

Indica la ruta donde se encuentra el archivo con la base de datos que relaciona los códigos oficiales del sistema estadístico nacional del mapa documentado para cada una de las divisiones geográficas del mapa, y los posibles nombres que pueden tomar estos códigos. La estructura del archivo debe ser la siguiente (ejemplo ilustrativo, el id no corresponde con el código oficial del DANE)

| id  | altname                   |
|-----|---------------------------|
| 1   | Amazonas                  |
| 1   | Departamento de Amazonas  |
| 1   | Amazonas (Colombia)       |
| 2   | Antioquia                 |
| 2   | Departamento de Antioquia |
| 2   | Antioquia (Colombia)      |
| 3   | Arauca                    |
| 3   | Departamento de Arauca    |
| 3   | Arauca (Colombia)         |
| ... | ...                       |

Es importante destacar que cada id puede tener múltiples nombres alternativos asociados, por lo que cada código se repetirá tantas veces como nombres alternativos tenga.

### level

El nivel de detalle del mapa se define mediante la variable level, que hace referencia al nivel de administración o ADM. Los ADM se utilizan para clasificar los límites administrativos y los datos geográficos asociados con ellos.

A continuación se describen los ADM más comunes y su aplicación:

-   ADM0: Este nivel se utiliza para hacer referencia al país completo o a una región más grande que lo incluya todo.
-   ADM1: Este nivel se utiliza para dividir el país en regiones más grandes, que pueden llamarse departamentos, provincias, estados, regiones o cualquier otro nombre que se ajuste a la división geográfica específica del país. Ejemplo: en el caso de Colombia, los departamentos se definen como ADM1.
-   ADM2: Este nivel se utiliza para dividir las regiones más grandes en sub-regiones o unidades más pequeñas, que pueden llamarse municipios, cantones, distritos, entre otros. Ejemplo: en el caso de Colombia, los municipios se definen como ADM2.
-   ADM3: Este nivel se utiliza para dividir las unidades más pequeñas en subdivisiones aún más pequeñas.

### regions_file (opcional)

Indica la ruta donde se encuentra el archivo con las regiones en las que se puede dividir el mapa que se está caracterizando, este archivo debe ser una tabla cuyos nombres de columna deben ser region_code, region_name, id y name.

La columna region_code debe contener el código único de cada región del mapa. La columna region_name debe contener el nombre de cada región del mapa. Las columnas id y name incluyen los códigos y nombres de cada división geográfica que deben coincidir con el sistema estadístico nacional.

Ejemplo de La estructura del archivo:

| region_code | region_name | id  | name               |
|-------------|-------------|-----|--------------------|
| orinoquia   | Orinoquia   | 50  | META               |
| pacifico    | Pacifico    | 50  | NARIÑO             |
| andina      | Andina      | 54  | NORTE DE SANTANDER |
| ...         | ...         | ... | ...                |

Es importante destacar que el identificador de cada región debe ser único y consistente con los archivos altnames_file y centroids_file.

### parent_map_name

Si el mapa caracterizado tiene un nivel de administración superior a ADM1, se debe poner el nombre del mapa que contiene la división geográfica ADM1, por ejemplo: para Colombia el meta.yaml de Municipios contiene su "padre" que es Departamentos.

### id_format

Indica por medio de una expresión regular el formato del código oficial del sistema estadístico nacional al que corresponda el mapa ingresado, por ejemplo: En caso de que el código de identificación fuera un string, como en el caso del ISO3 utilizado para los países del mundo ("ARG", "COL", "BRA", etc.), se podría utilizar la siguiente expresión regular para indicar el formato del código:

`id_format: "^[A-Z]{3}$"`

En este ejemplo, la expresión regular `^[A-Z]{3}$` indica que el código debe estar compuesto por tres letras mayúsculas.

En caso de que el código de identificación fuera un número, pero no tuviera un número fijo de dígitos, se podría utilizar la siguiente expresión regular para indicar el formato del código:

`id_format: "^\d+$"`

En este ejemplo, la expresión regular `^\d+$` indica que el código debe estar compuesto por uno o más dígitos.

Es importante destacar que el formato de identificación debe ser consistente con los archivos altnames_file, centroids_file y regions_file.

### code_cols

Arreglo de posibles nombres que pueden llegar a tener las variables que contienen el código oficial del sistema estadístico nacional.

### name_cols

Arreglo de posibles nombres que pueden llegar a tener las variables que contienen el nombre oficial del sistema estadístico nacional.

### topojson

Lista con las siguientes caracteristicas:

-   scope: Corresponde al área geográfica que se muestra en el mapa, el scope se utiliza para 'recortar' el mapa y mostrar sólo la porción del territorio que se desea visualizar. Un ejemplo es meta.yaml de departamentos de Colombia, el scope se define como depto, lo que significa que se mostrarán solamente los departamentos de Colombia en el mapa.

-   geographyName: nombre del objeto en el archivo topojson que contiene los nombres de las divisiones geográficas.

-   geographyId: nombre del objeto en el archivo topojson que contiene el geocode o indicador de las divisiones geográficas.

-   path: ruta que contiene el mapa en formato TopoJSON.

### projections

Las proyecciones en mapas se utilizan para convertir la superficie curva de la Tierra en una superficie plana para que se pueda visualizar en un mapa. Una proyección en mapa define cómo se mapea la superficie de la Tierra en un plano y cuáles son las distorsiones que se producen. El archivo YAML puede contener información sobre las distintas proyecciones que se desean realizar del mapa caracterizado, se debe ingresar en nombre de la proyección y definir los parámetros: centro, rotación, escala y traslación, a continuación una breve descripción de cada uno:

-   Centro: El centro de la proyección define el punto en el que se enfoca la vista del mapa. Este punto suele ser el centro de la región que se desea representar. Por ejemplo, en el caso de Colombia, el centro se ha definido como [-73.5, 4.2], lo que significa que el centro del mapa se encuentra en la longitud -73.5 y la latitud 4.2.

-   Rotación: La rotación de la proyección define el ángulo en que se gira el mapa en relación con su posición original. La rotación se expresa en grados y puede ser útil para mostrar ciertas regiones del mundo en ángulos más adecuados para su visualización. En el ejemplo de Colombia, la rotación se ha definido como [0, 0], lo que significa que el mapa no se ha girado.

-   Escala: La escala de la proyección define la relación entre las distancias en el mapa y las distancias en la realidad. La escala se puede expresar como una fracción, un porcentaje o una relación numérica. Por ejemplo, una escala de 1:100,000 significa que una unidad en el mapa representa 100,000 unidades en la realidad. En el ejemplo de Colombia, la escala se ha definido como 1.8, lo que significa que el mapa está ampliado en un 80% en comparación con el tamaño original.

-   Traslación: La traslación de la proyección define el desplazamiento del mapa en relación con su posición original. La traslación se puede expresar en píxeles o en coordenadas geográficas. En el ejemplo de Colombia, la traslación se ha definido como [0,0], lo que significa que el mapa no se ha desplazado.

Puede agregar más propiedades de su proyección si es necesario
