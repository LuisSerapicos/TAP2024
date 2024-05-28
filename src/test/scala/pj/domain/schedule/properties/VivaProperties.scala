package pj.domain.schedule.properties

import org.scalacheck.*
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, resourceId, resourceName, vivaStudent, vivaTitle}
import pj.domain.schedule.Utils.stringToDuration
import pj.domain.schedule.generators.AvailabilityGenerators.{generateAvailability, generateDuration, generatePreference}
import pj.domain.schedule.generators.ResourceGenerators.{generateResourceId, generateResourceName, generateThreeDigitNumber, generateUniqueResources}
import pj.domain.schedule.generators.VivaGenerators.{generateInstersectedAvailabilities, generateRoles, generateScheduledVivas, generateStudent, generateTitle, generateViva}

import java.time.{Duration, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap
import scala.util.Random

object VivaProperties extends Properties("VivaProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(500)

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd:HH:mm")
  val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
  val MIN_VIVAS = 1
  val MAX_VIVAS = 20

  //PROPERTIES NOTES
  //INPUTS E OUTPUTS VERIFICAR - ALGORITMO É NAS PROPERTIES

  
  property("Check if the viva is scheduled in the intervals in which its resources are available") = Prop.forAll(generateInstersectedAvailabilities) { scheduledVivas =>
    scheduledVivas.fold(_ => false, sv => {
      val teachers = sv._2.flatMap(_.availabilities)
      val externals = sv._3.flatMap(_.availabilities)
      val resources = teachers ++ externals
      
      sv._1.forall {
        case availability =>
          val isScheduledInAvailableInterval = resources.exists(a => (a.start.to.isBefore(availability.start.to) || a.start.to.isEqual(availability.start.to)) && (a.end.to.isAfter(availability.end.to) || a.end.to.isEqual(availability.end.to)))

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

          commonResources.isEmpty || (availability1.end.to.isBefore(availability2.start.to) || availability1.end.to.isEqual(availability2.start.to) || availability1.start.to.isAfter(availability2.end.to) || availability1.start.to.isEqual(availability2.end.to))
        }
      case _ => false
  }

  
  property("Check if there are no intersected scheduled vivas") = Prop.forAll(generateScheduledVivas) { scheduledVivas =>
    scheduledVivas.fold(_ => false, sv => {
      val scheduledVivas = sv._1
      scheduledVivas.forall { case vivaAvailability1@(viva1, availability1) =>
        val noOverlap = scheduledVivas.filterNot(_ == vivaAvailability1).forall { case (viva2, availability2) =>

          (availability1.end.to.isBefore(availability2.start.to) || availability1.end.to.isEqual(availability2.start.to) || availability1.start.to.isAfter(availability2.end.to) || availability1.start.to.isEqual(availability2.end.to))        }

        noOverlap
      }
    })
  }
  
  
  property("The generated resources should be unique") = Prop.forAll(generateInstersectedAvailabilities) { scheduledVivas =>
    scheduledVivas match
      case Right((_, teachers, externals, _)) =>
        val resources = teachers ++ externals
        val resourceIds = resources.map(_.id.to)

        resourceIds.distinct.sizeIs == resourceIds.sizeIs
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

          val totalPreference = resourceAvailabilities.map(_.preference.d).sum

          val scheduledVivasPreference = vivas.filter(_._1.roles.keys.toSeq.contains(resourceKey)).map { case (viva, availability) => availability.preference.d }.sum

          totalPreference == scheduledVivasPreference
        }
      case _ => false
  }


  property("Number of Roles must be at least 2 and at most 5") = Prop.forAll(for {
    duration <- generateDuration
    teachers <- generateUniqueResources(5, duration, Set.empty, true)
    externals <- generateUniqueResources(5, duration, teachers.map(_.id.to).toSet, false)
    roles <- generateRoles(teachers, externals)
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
    teachers <- generateUniqueResources(5, duration, Set.empty, true)
    externals <- generateUniqueResources(5, duration, teachers.map(_.id.to).toSet, false)
    roles <- generateRoles(teachers, externals)
    } yield roles) { roles =>
    roles.values.exists(_ == Role.President) && roles.values.exists(_ == Role.Advisor)
  }


  property("Generate ResourceId follows the correct pattern") = Prop.forAll(generateResourceId("T", Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceId: resourceId) =>
    //Check if resourceId follows the pattern "T\d{3}" or "E\d{3}"

    resourceId.to.matches("T\\d{3}") || resourceId.to.matches("E\\d{3}")
  }
  
  
  property("Generate ResourceName follows the correct pattern") = Prop.forAll(generateResourceName("Teacher ", Gen.oneOf(generateThreeDigitNumber).sample.getOrElse("0"))) { (resourceName: resourceName) =>
    //Check if resourceId follows the pattern "Teacher \d{3}" or "External \d{3}"

    resourceName.to.matches("Teacher \\d{3}") || resourceName.to.matches("External \\d{3}")
  }
  
  
  property("Generate Duration must be between 1 and 2 hours") = Prop.forAll(generateDuration) { (duration: agendaDuration) =>
    //println("Duration: " + duration)
    val durationTime = DateTimeFormatter.ofPattern("HH:mm:ss").format(LocalTime.parse(duration.to, timeFormatter))
    val duration2 = stringToDuration(durationTime).getOrElse(Duration.ZERO)
    duration2.toHours >= 1 && duration2.toHours <= 2
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

