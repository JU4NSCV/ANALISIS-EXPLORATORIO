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

    val conteoAdultFalse: Int = countBooleans("adult", false, valores)
    println("Conteo de falsos en Adults" + conteoAdultFalse)
    val conteoAdultTrue: Int = countBooleans("adult", true, valores)
    println("Conteo de verdaderos en Adults" + conteoAdultTrue)

    val maximoBudget: Double = maximo("budget",valores)
    println("El maximo de budget es: " + maximoBudget)
    val promBudget: Double = promedio("budget", valores)
    println("El promedio de budget es: " + promBudget)
    val minBudget: Double = minimo("budget", valores)
    println("El minimo de budget es: " + minBudget)
    val modaBudget: Map[Double, List[Double]] = moda("budget", valores)
    println("La moda de budget es: " + modaBudget)

    val conteoHomepage = modastring("homepage", valores)
    println("Conteo de frecuencia de Homepage es: " + conteoHomepage)

    val maximoId: Double = maximo("id", valores)
    println("El maximo de id es: " + maximoId)
    val promId: Double = promedio("id", valores)
    println("El promedio de id es: " + promId)
    val minId: Double = minimo("id", valores)
    println("El minimo de id es: " + minId)
    val modaId: Map[Double, List[Double]] = moda("id", valores)
    println("La moda de id es: " + modaId)

    val conteoImdb_id = modastring("imdb_id", valores)
    println("Conteo de frecuencia de imdb_id es: " + conteoImdb_id)

    val conteoOriginal_language = modastring("original_language", valores)
    println("Conteo de frecuencia de original_language es: " + conteoOriginal_language)

    val conteoOriginal_title = modastring("original_title", valores)
    println("Conteo de frecuencia de original_title es: " + conteoOriginal_title)

    val conteoOverview = modastring("overview", valores)
    println("Conteo de frecuencia de overview es: " + conteoOverview)

    val maximoPopularity: Double = maximo("popularity", valores)
    println("El maximo de popularity es: " + maximoPopularity)
    val promPopularity: Double = promedio("popularity", valores)
    println("El promedio de popularity es: " + promPopularity)
    val minPopularity: Double = minimo("popularity", valores)
    println("El minimo de popularity es: " + minPopularity)
    val modaPopularity: Map[Double, List[Double]] = moda("popularity", valores)
    println("La moda de popularity es: " + modaPopularity)

    val conteoPoster_path = modastring("poster_path", valores)
    println("Conteo de frecuencia de poster_path es: " + conteoPoster_path)

    val conteoRelease_date = modastring("release_date", valores)
    println("Conteo de frecuencia de release_date es: " + conteoRelease_date)

    val maximoRevenue: Double = maximo("revenue", valores)
    println("El maximo de revenue es: " + maximoRevenue)
    val promRevenue: Double = promedio("revenue", valores)
    println("El promedio de revenue es: " + promRevenue)
    val minRevenue: Double = minimo("revenue", valores)
    println("El minimo de revenue es: " + minRevenue)
    val modaRevenue: Map[Double, List[Double]] = moda("revenue", valores)
    println("La moda de revenue es: " + modaRevenue)

    val maximoRuntime: Double = maximo("runtime", valores)
    println("El maximo de runtime es: " + maximoRuntime)
    val promRuntime: Double = promedio("runtime", valores)
    println("El promedio de runtime es: " + promRuntime)
    val minRuntime: Double = minimo("runtime", valores)
    println("El minimo de runtime es: " + minRuntime)
    val modaRuntime: Map[Double, List[Double]] = moda("runtime", valores)
    println("La moda de runtime es: " + modaRuntime)

    val conteoStatus = modastring("status", valores)
    println("Conteo de frecuencia de status es: " + conteoStatus)

    val conteoTagline = modastring("tagline", valores)
    println("Conteo de frecuencia de tagline es: " + conteoTagline)

    val conteoTitle = modastring("title", valores)
    println("Conteo de frecuencia de title es: " + conteoTitle)

    val conteoVideoTrue: Int = countBooleans("video", true, valores)
    println("Conteo de verdaderos en video" +conteoVideoTrue)
    val conteoVideoFalse: Int = countBooleans("video", false, valores)
    println("Conteo de falsos en video" +conteoVideoFalse)

    val maximoVote_average: Double = maximo("vote_average", valores)
    println("El maximo de vote_average es: " + maximoVote_average)
    val promVote_average: Double = promedio("vote_average", valores)
    println("El promedio de vote_average es: " + promVote_average)
    val minVote_average: Double = minimo("vote_average", valores)
    println("El minimo de vote_average es: " + minVote_average)
    val modaVote_average: Map[Double, List[Double]] = moda("vote_average", valores)
    println("La moda de vote_average es: " + modaVote_average)

    val maximoVote_count: Double = maximo("vote_count", valores)
    println("El maximo de vote_count es: " + maximoVote_count)
    val promVote_count: Double = promedio("vote_count", valores)
    println("El promedio de vote_count es: " + promVote_count)
    val minVote_count: Double = minimo("vote_count", valores)
    println("El minimo de vote_count es: " + minVote_count)
    val modaVote_count: Map[Double, List[Double]] = moda("vote_count", valores)
    println("La moda de vote_count es: " + modaVote_count)

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
}