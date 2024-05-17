package pj.domain.schedule.properties

import org.scalacheck.*
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, resourceId, resourceName, vivaStudent, vivaTitle}
import pj.domain.schedule.Utils.stringToDuration
import pj.domain.schedule.generators.ResourceGenerators.generateResources
import pj.domain.schedule.generators.VivaGenerators.{generateRoles, generateViva}

import java.time.{Duration, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap
import scala.util.Random

object VivaProperties extends Properties("VivaProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd:HH:mm:ss")
  val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
  val MIN_VIVAS = 1
  val MAX_VIVAS = 20

  val uniqueNumbers = scala.util.Random.shuffle(0 to 999).toList
  //Generate sequence

  def generateThreeDigitNumber: Vector[String] =
    val random = new Random()
    val numbers = Random.shuffle((1 to 999).map(n => f"$n%03d")).take(999).toVector // Generate a shuffled sequence of unique 3-digit numbers as strings
    numbers


  /*
  property("Generate three digit number should be unique") = Prop.forAll(Gen.listOfN(1, generateThreeDigitNumber)) { numbers =>
    numbers.distinct.sizeIs == numbers.sizeIs
  }*/



  property("Number of Roles must be at least 2 and at most 5") = Prop.forAll(generateRoles) { roles =>
    println("Roles: " + roles)
    roles.sizeIs >= 2 || roles.sizeIs <= 5
  }


  property("Roles contain at least 1 President and 1 Advisor") = Prop.forAll(generateRoles) { roles =>
    //println("Roles: " + roles)
    roles.values.toList.contains(Role.President) && roles.values.toList.contains(Role.Advisor)
  }

  /*
  property("Generate ResourceId follows the correct pattern") = Prop.forAll(generateResourceId(Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceId: resourceId) =>
    //Check if resourceId follows the pattern "T\d{3}" or "E\d{3}"
    println("Resource Id: " + resourceId)
    resourceId.to.matches("T\\d{3}") || resourceId.to.matches("E\\d{3}")
  }


  property("Generate ResourceName follows the correct pattern") = Prop.forAll(generateResourceName(Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceName: resourceName) =>
    //Check if resourceId follows the pattern "Teacher \d{3}" or "External \d{3}"
    println("Resource Name: " + resourceName)
    resourceName.to.matches("Teacher \\d{3}") || resourceName.to.matches("External \\d{3}")
  }
  */

  /*
  property("Generate Duration must be between 1 and 2 hours") = Prop.forAll(generateDuration) { (duration: agendaDuration) =>
    //println("Duration: " + duration)
    //Agenda Duration must be min 1 hour and max 2 hours
    val durationTime = DateTimeFormatter.ofPattern("HH:mm:ss").format(LocalTime.parse(duration.to, timeFormatter))
    val duration2 = stringToDuration(durationTime).getOrElse(Duration.ZERO)
    duration2.toHours >= 1 && duration2.toHours <= 2
  }*/


  property("Generate vivas") = Prop.forAll(Gen.chooseNum(2, 5)) { (n: Int) =>
    val vivaListGen = Gen.listOfN(n, generateViva)
    vivaListGen.sample match
      case Some(vivaList) => vivaList.sizeIs == n
      case None => false
  }


  /*
  property("Generate Preference") = Prop.forAll(generatePreference) { preference =>
    preference.d >= 1 && preference.d <= 5
  }*/

  /*
  property("Generate Availability") = Prop.forAll(generateAvailability) { (availability: Availability) =>
    availability.start.to.toString.nonEmpty && availability.end.to.toString.nonEmpty && availability.preference != null
  }*/

  /*
  property("Availability start before end") = Prop.forAll(Gen.listOfN(1000, generateAvailability)) { availabilities =>
    availabilities.forall(a => a.start.to.isBefore(a.end.to))
  }*/

  property("Generate Resource") = Prop.forAll(generateResources) { resource =>
    println(resource)
    //check if resource id is not empty
    resource.id.to.nonEmpty
  }



  /*
  // Property: Resource Availability
  property("Resource Availability") = Prop.forAll(generateViva, generateResource) { (viva, resource) =>
    val vivaTime = viva.time
    resource.availabilities.exists(a => a.start.to.isBefore(vivaTime) && a.end.to.isAfter(vivaTime))
  }

  // Property: Viva Duration
  property("Viva Duration") = Prop.forAll(generateViva, generateResource) { (viva, resource) =>
    val vivaDuration = Duration.between(viva.startTime, viva.endTime)
    resource.availabilities.exists(a => Duration.between(a.start.to, a.end.to).compareTo(vivaDuration) >= 0)
  }

  // Property: Resource Role
  property("Resource Role") = Prop.forAll(generateViva) { viva =>
    val roles = viva.roles.values.toList
    roles.contains(Role.President) && roles.contains(Role.Advisor)
  }

  // Property: Resource Utilization
  property("Resource Utilization") = Prop.forAll(generateViva, generateResource) { (viva, resource) =>
    val totalAvailabilityDuration = resource.availabilities.map(a => Duration.between(a.start.to, a.end.to)).sum
    val vivaDuration = Duration.between(viva.startTime, viva.endTime)
    val utilization = vivaDuration.toDouble / totalAvailabilityDuration
    utilization >= 0.1 && utilization <= 0.9 // Assuming 10% to 90% as the acceptable utilization range
  }

  // Property: Viva Scheduling
  property("Viva Scheduling") = Prop.forAll(Gen.listOfN(10, generateViva)) { vivas =>
    val sortedVivas = vivas.sortBy(_.startTime)
    sortedVivas.sliding(2).forall {
      case List(v1, v2) => Duration.between(v1.endTime, v2.startTime).toMinutes <= 30 // Assuming 30 minutes as the maximum gap
      case _ => true
    }
  }

  // Property: Resource Preference
  property("Resource Preference") = Prop.forAll(generateViva, generateResource) { (viva, resource) =>
    resource.availabilities.find(a => a.start.to.isBefore(viva.startTime) && a.end.to.isAfter(viva.endTime)).exists(_.preference.d >= 3)
  }

  // Property: Unique Viva
  property("Unique Viva") = Prop.forAll(Gen.listOfN(10, generateViva)) { vivas =>
    val students = vivas.map(_.student)
    students.distinct.size == students.size
  }
  */

