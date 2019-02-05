package decliner

import utest._

object NameTestSuite extends TestSuite {
  import Decliner._

  val tests = Tests {
    "First name" - {
      check(declineFirstName("Иван", Nominative), "Иван")
      check(declineFirstName("Иван", Genitive), "Ивана")
      check(declineFirstName("Иван", Dative), "Ивану")
      check(declineFirstName("Иван", Accusative), "Ивана")
      check(declineFirstName("Иван", Instrumental), "Иваном")
      check(declineFirstName("Иван", Prepositional), "Иване")

      check(declineFirstName("Мария", Nominative), "Мария")
      check(declineFirstName("Мария", Genitive), "Марии")
      check(declineFirstName("Мария", Dative), "Марии")
      check(declineFirstName("Мария", Accusative), "Марию")
      check(declineFirstName("Мария", Instrumental), "Марией")
      check(declineFirstName("Мария", Prepositional), "Марии")
    }

    "Middle name" - {
      check(declineMiddleName("Петрович", Nominative), "Петрович")
      check(declineMiddleName("Петрович", Genitive), "Петровича")
      check(declineMiddleName("Петрович", Dative), "Петровичу")
      check(declineMiddleName("Петрович", Accusative), "Петровича")
      check(declineMiddleName("Петрович", Instrumental), "Петровичем")
      check(declineMiddleName("Петрович", Prepositional), "Петровиче")

      check(declineMiddleName("Анатольевна", Nominative), "Анатольевна")
      check(declineMiddleName("Анатольевна", Genitive), "Анатольевны")
      check(declineMiddleName("Анатольевна", Dative), "Анатольевне")
      check(declineMiddleName("Анатольевна", Accusative), "Анатольевну")
      check(declineMiddleName("Анатольевна", Instrumental), "Анатольевной")
      check(declineMiddleName("Анатольевна", Prepositional), "Анатольевне")
    }

    "Last name" - {
      check(declineLastName("Кузнецов", Nominative), "Кузнецов")
      check(declineLastName("Кузнецов", Genitive), "Кузнецова")
      check(declineLastName("Кузнецов", Dative), "Кузнецову")
      check(declineLastName("Кузнецов", Accusative), "Кузнецова")
      check(declineLastName("Кузнецов", Instrumental), "Кузнецовым")
      check(declineLastName("Кузнецов", Prepositional), "Кузнецове")

      check(declineLastName("Павлова", Nominative), "Павлова")
      check(declineLastName("Павлова", Genitive), "Павловой")
      check(declineLastName("Павлова", Dative), "Павловой")
      check(declineLastName("Павлова", Accusative), "Павлову")
      check(declineLastName("Павлова", Instrumental), "Павловой")
      check(declineLastName("Павлова", Prepositional), "Павловой")
    }
  }

  def check(declined: Option[String], correct: String): Unit = {
    assert(declined.isDefined)
    declined.foreach(_ ==> correct)
  }
}
