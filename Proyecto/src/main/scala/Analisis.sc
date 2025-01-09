import com.github.tototoshi.csv.*

import java.io.File

implicit object CSVFormatter extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

val path :String = "C:\\Users\\sebas\\OneDrive\\Documentos\\U\\PRACTICUM\\SMALL\\pi_movies_small.csv"
val reader = CSVReader.open(new File(path))

val valores = reader.allWithHeaders()

valores.foreach(println)
