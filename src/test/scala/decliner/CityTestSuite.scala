package decliner

import utest._

import scala.io.Source

object CityTestSuite extends TestSuite {
  val tests = Tests {
    "Main russian cities" - {
      check(Decliner.cityTo("Флоренция"), "Флоренцию")

      check(Decliner.cityIn("Москва"), "Москве")
      check(Decliner.cityFrom("Москва"), "Москвы")
      check(Decliner.cityTo("Москва"), "Москву")

      check(Decliner.cityIn("Санкт-Петербург"), "Санкт-Петербурге")
      check(Decliner.cityFrom("Санкт-Петербург"), "Санкт-Петербурга")
      check(Decliner.cityTo("Санкт-Петербург"), "Санкт-Петербург")
    }

    "Russian cities" - {
      checkFile("russian_cities.txt")
    }

    "Kazakh cities" - {
      checkFile("kazakh_cities.txt")
    }

    "World cities" - {
      checkFile("world_cities.txt")
    }

    "Various cities" - {
      checkFile("various_cities.txt")
    }
  }

  def checkFile(fileName: String) = Source.fromResource(fileName).getLines().foreach(check)

  def check(line: String): Unit = {
    val forms = line.split(", ")
    assert(forms.length == 4)
    val Array(nom, in, from, to) = forms
    Decliner.cityIn(nom).foreach(_ ==> in)
    Decliner.cityFrom(nom).foreach(_ ==> from)
    Decliner.cityTo(nom).foreach(_ ==> to)
  }

  def check(declined: Option[String], correct: String): Unit = {
    assert(declined.isDefined)
    declined.foreach(_ ==> correct)
  }
}
