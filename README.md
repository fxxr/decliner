## Decliner

A Scala port of the Javascript library [lvovich](https://github.com/nodkz/lvovich) for declining people names and city names in Russian and detecting the gender by name.

### Usage

#### Decline people names

```
import Decliner._

declineFirstName("Иван", Genitive)      // returns "Ивана"
declineFirstName("Мария", Instrumental) // returns "Марией"
declineMiddleName("Петрович", Prepositional) // returns "Петровиче"
declineLastName("Кузнецов", Instrumental) // returns "Кузнецовым"
```

#### Decline city names

```
import Decliner._

cityIn("Москва")    // returns "Москве"
cityFrom("Москва")  // returns "Москвы"
cityTo("Москва")    // returns "Москву"
```

#### Detect the gender

```
Gender.byFirstName("Иван").exists(_.isMale)
Gender.byMiddleName("Петровна").exists(_.isFemale)
Gender.byLastName("Дарвин").exists(_.isAndrogynous)
Gender.byFullName("Петрова", "Мария", "Евгеньевна").exists(_.isFemale)
```