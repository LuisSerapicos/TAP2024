package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate}
import pj.domain.schedule.Utils.{dateFormatter, fixAvailabilities, parseDuration}
import pj.domain.schedule.Domain.{Agenda, Availability2, Resource, Role2, Viva, VivaDuration}
import java.time.Duration
import scala.xml.{Elem, Node}

object newAlgorithm:



  /**
   * This function finds the best time for a viva by checking the intersection of availabilities of all resources.
   * It returns a time slot that is compatible with all the other resources.
   *
   * @param currentResource The resource whose availability is being checked.
   * @param otherResources  The list of other resources.
   * @param duration        The duration of the viva.
   * @return An optional Availability instance representing the best time for the viva.
   */
  def intersectAvailabilities(currentResource: Resource, otherResources: List[Resource], duration: agendaDuration): List[Availability2] =
    val durationF = parseDuration(duration)
    val a1 = currentResource.availabilities.sortBy(availability => availabilityDate.toLocalDateTime(availability.start))
    val a2 = otherResources.map(resource => resource.availabilities.sortBy(availability => availabilityDate.toLocalDateTime(availability.start)))

    val possibleAvailabilities = a1.filter { availability =>
      a2.forall { otherAvailabilities =>
        otherAvailabilities.exists { otherAvailability =>
          val overlapStart = if (availabilityDate.toLocalDateTime(availability.start).isAfter(availabilityDate.toLocalDateTime(otherAvailability.start)))
            availability.start
          else
            otherAvailability.start

          val overlapEnd = if (availabilityDate.toLocalDateTime(availability.end).isBefore(availabilityDate.toLocalDateTime(otherAvailability.end)))
            availability.end
          else
            otherAvailability.end

          Duration.between(availabilityDate.toLocalDateTime(overlapStart), availabilityDate.toLocalDateTime(overlapEnd)).toMinutes >= durationF.toMinutes
        }
      }
    }
    println("Possible Availabilities: " + possibleAvailabilities)

    possibleAvailabilities


  /**
   * This function checks if a resource is available for a given time interval.
   * It returns the resource and the availabilities that are available.
   *
   * @param viva     The viva for which the resource availability is being checked.
   * @param duration The duration of the viva.
   * @return A Result instance containing either a tuple of the viva and a list of available Availability instances, or a DomainError.
   */
  def isResourceAvailable(viva: Viva, duration: agendaDuration): Result[(Viva, List[Availability2])] =
    val requiredResources = viva.roles.filterNot(_._2 == Role2.None)
    val suitableAvailabilities = requiredResources.flatMap { resource =>
      intersectAvailabilities(resource, requiredResources.filterNot(_ == resource), duration)
    }
    println("Suitable Availabilities: " + suitableAvailabilities)



    if suitableAvailabilities.isEmpty then
      Left(DomainError.ImpossibleSchedule)
    else
      findBestTimeSlot(suitableAvailabilities, duration) match
        case Some((totalPreference, timeSlots)) =>
          Right((viva, timeSlots))


  def findBestTimeSlot(suitableAvailabilities: List[Availability2], duration: agendaDuration): Option[(Int, List[Availability2])] =
    val durationF = parseDuration(duration)

    val combinations = suitableAvailabilities.combinations(3).toList

    val overlappingCombinations = combinations.filter { combination =>
      val sortedAvailabilities = combination.sortBy(availability => availabilityDate.toLocalDateTime(availability.start))
      sortedAvailabilities.sliding(2).forall:
        case List(a, b) => availabilityDate.toLocalDateTime(a.end).isAfter(availabilityDate.toLocalDateTime(b.start))
        case _ => true
    }

    val combinationPreferences = overlappingCombinations.map { combination =>
      val totalPreference = combination.map(_.preference.to).sum
      (totalPreference, combination)
    }

    println("Combination Preferences: " + combinationPreferences)

    combinationPreferences.maxByOption(_._1)


  /**
   * This function finds the best time for a viva by checking the latest time slot with the highest preference.
   * It returns the viva and the time slot with the highest preference.
   *
   * @param viva                   The viva for which the best time is being found.
   * @param suitableAvailabilities The list of suitable availabilities.
   * @param duration               The duration of the viva.
   * @return An optional tuple containing the viva and a tuple of the start time, end time, and preference of the time slot.
   */
  def findLatestTimeSlot(viva: Viva, suitableAvailabilities: List[Availability2], duration: agendaDuration): Option[(Viva, (availabilityDate, availabilityDate, Int))] =
    val durationF = parseDuration(duration)
    val starts = suitableAvailabilities.map(_.start)
    println("Starts: " + starts)
    val latestTimeSlot = starts.maxByOption(availabilityDate.toLocalDateTime)
    println("Latest Time Slot: " + latestTimeSlot)
    latestTimeSlot.map { modifiedLatestTimeSlot =>
      val start = modifiedLatestTimeSlot
      val end = modifiedLatestTimeSlot.plusTime(durationF)
      val preference = suitableAvailabilities.map(a => a.preference.to).sum
      println("Overlap: " + start + " - " + end + " Preference: " + preference)

      (viva, (start, end, preference))
    }




  /**
   * This function updates the required resources after a viva has been scheduled.
   * It returns the updated required resources and the list of vivas with their time slots.
   *
   * @param vivas     The list of vivas.
   * @param resources The list of resources.
   * @param duration  The duration of the viva.
   * @return A Result instance containing either a tuple of the list of updated resources and a list of tuples of the viva and its time slot, or a DomainError.
   */
  def scheduleAllVivas(vivas: List[Viva], resources: List[Resource], duration: agendaDuration): Result[(List[Resource], List[(Viva, (availabilityDate, availabilityDate, Int))])] =
    vivas.foldLeft[Result[(List[Resource], List[(Viva, (availabilityDate, availabilityDate, Int))])]](Right((resources, List.empty[(Viva, (availabilityDate, availabilityDate, Int))]))) { case (result, viva) =>
      result.flatMap { case (rl, vl) =>
        val updatedViva = viva.copy(roles = viva.roles.map { resource =>
          val updatedAvailabilities = rl.find(_.id == resource.id).map(_.availabilities).getOrElse(resource.availabilities)
          resource.copy(availabilities = updatedAvailabilities)
        })

        isResourceAvailable(updatedViva, duration) match
          case Right((viva, availabilities)) =>
            findLatestTimeSlot(viva, availabilities, duration) match
              case Some((viva, timeSlot)) =>
                val updatedResources = rl.map { resource =>
                  if (updatedViva.roles.map(_.id).contains(resource.id))
                    val newAvailabilities = resource.availabilities.flatMap { availability =>
                      availabilities.contains(availability) match
                        case true =>
                          val updatedAvailability = fixAvailabilities(availability, timeSlot._1, timeSlot._2)
                          updatedAvailability
                        case false => List(availability)
                    }
                    resource.copy(availabilities = newAvailabilities)
                  else
                    resource
                }

                Right((updatedResources, (viva, timeSlot) :: vl))
              case None =>
                Left(DomainError.ImpossibleSchedule)
          case Left(error) =>
            Left(error)
      }
    }


  /**
   * This function processes the agenda and vivas.
   * It returns the list of outputs.
   *
   * @param duration The duration of the viva.
   * @param vivas    The list of vivas.
   * @return A Result instance containing either a list of Agendas, or a DomainError.
   */
  def scheduleVivas(duration: Result[VivaDuration], vivas: Result[List[Viva]]): Result[List[Agenda]] =
    duration match
      case Right(durationF) =>
        vivas match
          case Right(vivasF) =>
            val requiredResources = vivasF.flatMap(_.roles)
            val scheduledVivas = scheduleAllVivas(vivasF, requiredResources, durationF.duration)

            scheduledVivas match
              case Right((resources, sVivas)) =>
                println("Resources: " + resources)
                val agenda = sVivas.map { case (viva, sViva) =>
                  Agenda.create(viva, sViva._1, sViva._2, sViva._3)
                }
                Right(agenda)

              case Left(error) => Left(error)
          case Left(error) => Left(error)
      case Left(error) => Left(error)


  /**
   * This function takes an XML Node, parses it to extract the necessary information,
   * schedules the vivas using the `newAlgorithm.scheduleVivas` function, and then
   * generates an XML output based on the result.
   *
   * @param xml The XML Node that contains the data for scheduling the vivas.
   * @return A Result[Elem] which is a Right[Elem] if the operation is successful,
   *         or a Left[DomainError] if an error occurs. The Elem in the Right case
   *         is the XML output of the scheduled vivas.
   */
  def scheduleVivasXML(xml: Node): Result[Elem] =
    val agendaResult = XMLtoDomain.getVivaDuration(xml)
    val result = XMLtoDomain.getVivas(xml)

    (result, agendaResult) match
      case (Left(error), _) => Left(error)
      case (_, Left(error)) => Left(error)
      case (Right(vivas: List[Viva]), Right(agenda)) =>
        val vivaOutputs = newAlgorithm.scheduleVivas(agendaResult, result)

        vivaOutputs match
          case Left(error) => Left(error)
          case Right(output) =>

            val sumPreferences = output.map(_.totalVivaPreference).sum
            Right(
              <schedule xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../schedule.xsd"
                        totalPreference={sumPreferences.toString}>
                {output.sortBy(vivaOutput => availabilityDate.toLocalDateTime(vivaOutput.start)).map(out =>
                <viva student={out.viva.student.to} title={out.viva.title.to} start={dateFormatter(availabilityDate.toLocalDateTime(out.start))} end={dateFormatter(availabilityDate.toLocalDateTime(out.end))} preference={out.totalVivaPreference.toString}>
                  {out.viva.roles.sortBy(_.role).flatMap(resource =>
                  resource.role match {
                    case Role2.President => Some(<president name={resource.name.to}/>)
                    case Role2.Advisor => Some(<advisor name={resource.name.to}/>)
                    case Role2.Coadvisor => Some(<coadvisor name={resource.name.to}/>)
                    case Role2.Supervisor => Some(<supervisor name={resource.name.to}/>)
                    case _ => None
                  })}
                </viva>)}
              </schedule>
            )

