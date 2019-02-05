package decliner

import utest._

import scala.io.Source

object GenderTestSuite extends TestSuite {
  val tests = Tests {
    "Simple names" - {
      assert(Gender.byFirstName("Иван").exists(_.isMale))
      assert(Gender.byFirstName("Мария").exists(_.isFemale))
      assert(Gender.byFirstName("Саша").exists(_.isAndrogynous))
      assert(Gender.byFirstName("БезИмени").isEmpty)
      assert(Gender.byMiddleName("Иванович").exists(_.isMale))
      assert(Gender.byMiddleName("Петровна").exists(_.isFemale))
      assert(Gender.byMiddleName("БезОтчества").isEmpty)
      assert(Gender.byLastName("Иванов").exists(_.isMale))
      assert(Gender.byLastName("Петрова").exists(_.isFemale))
      assert(Gender.byLastName("Дарвин").exists(_.isAndrogynous))
      assert(Gender.byLastName("БезФамилии").isEmpty)
    }

    "Full names" - {
      assert(Gender.byFullName("Иванов", "Иван", "Иванович").exists(_.isMale))
      assert(Gender.byFullName("Петрова", "Мария", "Евгеньевна").exists(_.isFemale))
      assert(Gender.byFullName("Сидорова", "Иван", "Юрьевич").isEmpty)
      assert(Gender.byFullName("Игнатов", "Валентина", "Валерьевна").isEmpty)
    }

    "Males" - {
      Source.fromResource("males.txt").getLines()
        .foreach(maleName => assert(Gender.byFirstName(maleName).exists(_.isMale)))
    }

    "Females" - {
      Source.fromResource("females.txt").getLines()
        .foreach(femaleName => assert(Gender.byFirstName(femaleName).exists(_.isFemale)))
    }
  }
}
