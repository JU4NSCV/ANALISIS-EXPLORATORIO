class Project

import com.github.tototoshi.csv.*

import java.io.File

object Reader{
  def main(args: Array[String]): Unit = {
    implicit object CSVFormatter extends DefaultCSVFormat {
      override val delimiter: Char = ';'
    }

    val path: String = "src/main/pi_movies_small.csv"
    val reader = CSVReader.open(new File(path))

    val valores:List[Map[String,String]] = reader.allWithHeaders()

    // valores.foreach(println)
    
    println(presentarDatosBooleanos("adult",valores))
    println(presentarDatosNumericos("budget", valores))
    println(presentarDatosString("homepage", valores))
    println(presentarDatosNumericos("id", valores))
    println(presentarDatosString("imdb_id", valores))
    println(presentarDatosString("original_language", valores))
    println(presentarDatosString("original_title", valores))
    println(presentarDatosString("overview", valores))
    println(presentarDatosNumericos("popularity", valores))
    println(presentarDatosString("poster_path", valores))
    println(presentarDatosString("release_date", valores))
    println(presentarDatosNumericos("revenue", valores))
    println(presentarDatosNumericos("runtime", valores))
    println(presentarDatosString("status", valores))
    println(presentarDatosString("tagline", valores))
    println(presentarDatosString("title",valores))
    println(presentarDatosBooleanos("video",valores))
    println(presentarDatosNumericos("vote_average",valores))
    println(presentarDatosNumericos("vote_count",valores))
  }

  def countBooleans (columna:String, validar:Boolean, datos:List[Map[String,String]]): Int = {
    val aux:List[Boolean] = datos.map(x => x(columna).toBoolean).filter(_ == validar)
    aux.length
  }
  def maximo (columna:String, datos:List[Map[String,String]]): Double = {
    val aux: Double = datos.map(x => x(columna).toDouble).max
    aux
  }

  def promedio (columna: String, datos: List[Map[String, String]]): Double = {
    val aux: List[Double] = datos.map(x => x(columna).toDouble)
    aux.sum / aux.length.toDouble
  }

  def minimo (columna: String, datos: List[Map[String, String]]): Double = {
    val aux: Double = datos.map(x => x(columna).toDouble).min
    aux
  }

  def moda(columna: String, datos: List[Map[String, String]]): Map[Double, List[Double]] = {
    val aux: Map[Double, List[Double]] = datos.map(x => x(columna).toDouble)
      .groupBy(x => x)
    aux
  }

  def modastring(columna: String, datos: List[Map[String, String]]): Map[String, List[String]] = {
    val aux: Map[String, List[String]] = datos.map(x => x(columna))
      .groupBy(x => x)
    aux
  }

  def desviacionSTD(columna: String, datos: List[Map[String, String]]): Double = {
    val data = datos.map(x => x(columna).toDouble)
    val desviacion = data.map(x => Math.pow(x - data.sum / data.size.toDouble, 2)).sum / data.size.toDouble
    Math.sqrt(desviacion)
  }
  def presentarDatosNumericos (columna: String, datos: List[Map[String, String]]): String = {
    val txt: String = s"El maximo de $columna es: " + maximo(columna, datos) + "\n" + s"El promedio de $columna es: " + promedio(columna, datos) + "\n"
                      + s"El minimo de $columna " + minimo(columna, datos) + "\n" + s"La moda de $columna es: " + moda(columna, datos) + "\n"
                        + s"La desviacion estandar de $columna es: " + desviacionSTD(columna, datos) + "\n"
    txt
  }
  
  def presentarDatosBooleanos (columna: String, datos: List[Map[String, String]]): String = {
    val txt: String = s"Conteo de falsos en $columna " + countBooleans(columna, false, datos) + "\n" 
    + s"Conteo de verdaderos en Adults " + countBooleans(columna, true, datos) + "\n"
    txt
  }
  
  def presentarDatosString (columna: String, datos: List[Map[String, String]]): String = {
    val txt: String = s"Conteo de frecuencia de $columna es: " + modastring(columna, datos) + "\n"
    txt
  }
}