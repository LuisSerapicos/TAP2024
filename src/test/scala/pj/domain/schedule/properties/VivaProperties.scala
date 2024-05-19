package pj.domain.schedule.properties

import org.scalacheck.*
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, resourceId, resourceName, vivaStudent, vivaTitle}
import pj.domain.schedule.Utils.stringToDuration
import pj.domain.schedule.generators.AvailabilityGenerators.{generateAvailability, generateDuration, generatePreference}
import pj.domain.schedule.generators.ResourceGenerators.{generateResourceId, generateResourceName, generateResources, generateThreeDigitNumber}
import pj.domain.schedule.generators.VivaGenerators.{generateInstersectedAvailabilities, generateRoles, generateScheduledVivas, generateStudent, generateTitle, generateViva}

import java.time.{Duration, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap
import scala.util.Random

object VivaProperties extends Properties("VivaProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd:HH:mm:ss")
  val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
  val MIN_VIVAS = 1
  val MAX_VIVAS = 20

  
  property("Check if the viva is scheduled in the intervals in which its resources are available") = Prop.forAll(generateInstersectedAvailabilities) { scheduledVivas =>
    scheduledVivas.fold(_ => false, sv => {
      val teachers = sv._2.flatMap(_.availabilities)
      val externals = sv._3.flatMap(_.availabilities)
      val resources = teachers ++ externals
      
      sv._1.forall {
        case availability =>
          val isScheduledInAvailableInterval = resources.exists(a => (a.start.to.isBefore(availability.start.to) || a.start.to.isEqual(availability.start.to)) && (a.end.to.isAfter(availability.end.to) || a.end.to.isEqual(availability.end.to)))
          if (!isScheduledInAvailableInterval) {
            println("Availability: " + availability)
            println("Resources: " + resources)
          }
          isScheduledInAvailableInterval
      }
    })
  }


  property("Check if resources are not overlapped within scheduled vivas") = Prop.forAllNoShrink(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas match
      case Right((vivas, _, _, _)) =>
        val resources = vivas.map { case (viva, availability) => (viva.roles.keys.toSeq, availability) }

        resources.combinations(2).forall { case Seq((resourceKeys1, availability1), (resourceKeys2, availability2)) =>
          val commonResources = resourceKeys1.intersect(resourceKeys2)
          println("Resource Keys 1: " + resourceKeys1)
          println("Resource Keys 2: " + resourceKeys2)
          println("Availability 1: " + availability1)
          println("Availability 2: " + availability2)
          println("Common Resources: " + commonResources)
          commonResources.isEmpty || (availability1.end.to.isBefore(availability2.start.to) || availability1.end.to.isEqual(availability2.start.to) || availability1.start.to.isAfter(availability2.end.to) || availability1.start.to.isEqual(availability2.end.to))
        }
      case _ => false
  }

  
  property("Check if there are no intersected scheduled vivas") = Prop.forAll(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas.fold(_ => false, sv => {
      val scheduledVivas = sv._1
      scheduledVivas.forall { case vivaAvailability1@(viva1, availability1) =>
        val noOverlap = scheduledVivas.filterNot(_ == vivaAvailability1).forall { case (viva2, availability2) =>
          println("Viva 1: " + viva1)
          println("Availability 1: " + availability1)
          println("Viva 2: " + viva2)
          println("Availability 2: " + availability2)
          (availability1.end.to.isBefore(availability2.start.to) || availability1.end.to.isEqual(availability2.start.to) || availability1.start.to.isAfter(availability2.end.to) || availability1.start.to.isEqual(availability2.end.to))        }
        if (!noOverlap) {
        }
        noOverlap
      }
    })
  }
  
  
  property("The generated resources should unique") = Prop.forAll(generateInstersectedAvailabilities) { scheduledVivas =>
    scheduledVivas match
      case Right((vivas, teachers, externals, _)) =>
        val resources = teachers ++ externals
        val resourceIds = resources.map(_.id.to)
        println("Resource Ids: " + resourceIds)
        resourceIds.distinct.sizeIs == resourceIds.sizeIs
      case _ => false
  }


  property("Check if the number of roles in the schedule viva is the same as the initial viva resources") = Prop.forAllNoShrink(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas match
      case Right((vivas, teachers, externals, _)) =>
        println("Teachers: " + teachers)
        println("Externals: " + externals)
        val resources = vivas.map { case (viva, availability) => (viva.roles.keys.toSeq, availability) }
        resources.forall { case (resourceKeys, _) =>
          println("Resource Keys: " + resourceKeys)
          val allResources = teachers ++ externals
          println("All Resources: " + allResources)
          println("All Resources Size: " + allResources.size)
          println("Resource Keys Size: " + resourceKeys.size)
          resourceKeys.sizeIs == allResources.sizeIs
        }
      case _ => false
  }


  property("Check if one or more availabilities follow the minimum duration") = Prop.forAll(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas match
      case Right((vivas, _, _, _)) =>
        val resources = vivas.map { case (viva, availability) => (viva, availability) }
        resources.forall { case (viva, availability) =>
          val vivaDuration = Duration.between(availability.start.to, availability.end.to)
          availability.end.to.isAfter(availability.start.to) && Duration.between(availability.start.to, availability.end.to).compareTo(vivaDuration) >= 0
        }
      case _ => false
  }


  property("Check if the total preference of the scheduled vivas is the same as the algorithm calculations") = Prop.forAll(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas match
      case Right((vivas, teachers, externals, _)) =>
        val requiredResources = vivas.map { case (viva, availability) => (viva.roles.keys.toSeq, availability) }
        requiredResources.flatMap(_._1).distinct.forall { resourceKey =>
          val resource = teachers.find(_.id.to.equals(resourceKey)).getOrElse(externals.find(_.id.to.equals(resourceKey)).getOrElse(???))
          val resourceAvailabilities = requiredResources.filter(_._1.contains(resourceKey)).map(_._2)
          println("Resource: " + resource)
          println("Resource Availabilities: " + resourceAvailabilities)
          val totalPreference = resourceAvailabilities.map(_.preference.d).sum
          println("Total Preference: " + totalPreference)
          val scheduledVivasPreference = vivas.filter(_._1.roles.keys.toSeq.contains(resourceKey)).map { case (viva, availability) => availability.preference.d }.sum
          println("Scheduled Vivas Preference: " + scheduledVivasPreference)
          totalPreference == scheduledVivasPreference
        }
      case _ => false
  }


  property("Number of Roles must be at least 2 and at most 5") = Prop.forAll(for {
    duration <- generateDuration
    resources <- Gen.listOfN(5, generateResources(duration))
    roles <- generateRoles(resources)
  } yield roles) { roles =>
    roles.sizeIs >= 2 && roles.sizeIs <= 5
  }
  

  property("Generate three digit number should be unique") = Prop.forAll(Gen.listOfN(1, generateThreeDigitNumber)) { numbers =>
    numbers.distinct.sizeIs == numbers.sizeIs
  }

  
  property("Generate Student Name should follow the pattern 'Student {3}'") = Prop.forAll(generateStudent) { student =>
    student.to.matches("Student \\d{3}")
  }
  
  
  property("Generate Viva Title should follow the pattern 'Title {3}'") = Prop.forAll(generateTitle) { title =>
    title.to.matches("Title \\d{3}")
  }
  
  
  property("Roles contain at least 1 President and 1 Advisor") = Prop.forAll(for {
    duration <- generateDuration
    resources <- Gen.listOfN(5, generateResources(duration))
    roles <- generateRoles(resources)
    } yield roles) { roles =>
    roles.values.toList.contains(Role.President) && roles.values.toList.contains(Role.Advisor)
  }
  
  
  property("Generate ResourceId follows the correct pattern") = Prop.forAll(generateResourceId("T", Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceId: resourceId) =>
    //Check if resourceId follows the pattern "T\d{3}" or "E\d{3}"
    println("Resource Id: " + resourceId)
    resourceId.to.matches("T\\d{3}") || resourceId.to.matches("E\\d{3}")
  }
  
  
  property("Generate ResourceName follows the correct pattern") = Prop.forAll(generateResourceName("Teacher ", Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceName: resourceName) =>
    //Check if resourceId follows the pattern "Teacher \d{3}" or "External \d{3}"
    println("Resource Name: " + resourceName)
    resourceName.to.matches("Teacher \\d{3}") || resourceName.to.matches("External \\d{3}")
  }
  
  
  property("Generate Duration must be between 1 and 2 hours") = Prop.forAll(generateDuration) { (duration: agendaDuration) =>
    //println("Duration: " + duration)
    //Agenda Duration must be min 1 hour and max 2 hours
    val durationTime = DateTimeFormatter.ofPattern("HH:mm:ss").format(LocalTime.parse(duration.to, timeFormatter))
    val duration2 = stringToDuration(durationTime).getOrElse(Duration.ZERO)
    duration2.toHours >= 1 && duration2.toHours <= 2
  }
  
  
  property("Generate vivas") = Prop.forAll(Gen.chooseNum(2, 5)) { (n: Int) =>
    val vivaListGen = Gen.listOfN(n, generateViva)
    vivaListGen.sample match
      case Some(vivaList) => vivaList.sizeIs == n
      case None => false
  }
  
  
  property("Generate Preference") = Prop.forAll(generatePreference) { preference =>
    preference.d >= 1 && preference.d <= 5
  }
  
  
  property("Generate Availability") = Prop.forAll(generateDuration.flatMap(duration => generateAvailability(duration))) { availability =>
    availability.start.to.toString.nonEmpty && availability.end.to.toString.nonEmpty
  }
  
  
  property("Availability start before end") = Prop.forAll(generateDuration.flatMap(duration => Gen.listOfN(1000, generateAvailability(duration)))) { availabilities =>
    availabilities.forall { availability =>
      availability.start.to.isBefore(availability.end.to)
    }
  }

