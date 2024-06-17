package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate}
import pj.domain.schedule.Utils.{anyOverlap, dateFormatter, fixAvailabilities, generateTimeSlots, parseDuration, sequence}
import pj.domain.schedule.Domain.{Agenda, Availability2, Resource, Role2, Viva, VivaDuration}

import java.time.Duration
import scala.xml.{Elem, Node}

object Algorithm:


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
      findAllTimeSlots(suitableAvailabilities, duration, requiredResources.length) match
        case head :: tail =>
          Right((viva, head._2))
        case Nil => Left(DomainError.ImpossibleSchedule)


  /**
   * This function finds all possible time slots for a given number of resources.
   * It returns a list of tuples containing the total preference and the list of time slots.
   * @param suitableAvailabilities
   * @param duration
   * @param numResources
   * @return A list of tuples containing the total preference and the list of time slots.
   */
  def findAllTimeSlots(suitableAvailabilities: List[Availability2], duration: agendaDuration, numResources: Int): List[(Int, List[Availability2])] =
    val durationF = parseDuration(duration)

    val combinations = suitableAvailabilities.combinations(numResources).toList

    val overlappingCombinations = combinations.filter { combination =>
      val sortedAvailabilities = combination.sortBy(availability => availabilityDate.toLocalDateTime(availability.start))
      sortedAvailabilities.sliding(2).forall:
        case List(a, b) => availabilityDate.toLocalDateTime(a.end).isAfter(availabilityDate.toLocalDateTime(b.start))
        case _ => true
    }

    val validCombinations = overlappingCombinations.flatMap { combination =>
      findLatestTimeSlotWithNoViva(combination, duration).map { timeSlots =>
        timeSlots.map { case (start, end, preference) =>
          (preference, List(Availability2(start, end, Preference(preference))))
        }
      }.getOrElse(List.empty[(Int, List[Availability2])])
    }

    println("Valid Combinations: " + validCombinations)

    validCombinations



/**
   * This function finds the global max preference for the vivas. It returns the total preference and the list of vivas with their time slots.
   * It does this by generating all possible combinations of time slots for each viva and then selecting the combination with the highest total preference.
   * @param vivas
   * @param allTimeSlots
   * @return A tuple containing the total preference and the list of vivas with their time slots.
   */
  def findGlobalPreference(vivas: Result[List[Viva]], allTimeSlots: List[List[(Int, List[Availability2])]]): (Int, List[(Viva, List[Availability2])]) =
    vivas match
      case Right(vivas) =>
        val allCombinationsPerViva = allTimeSlots.map(_.map(List(_)))   //Generate all combinations per viva

        val allCombinations = sequence(allCombinationsPerViva).map(vivas.zip(_)).filterNot(anyOverlap)

        // Reunite the availabilities that dont overlap
        val validCombinations = allCombinations.filter { combination =>
          val sortedCombination = combination.flatMap(_._2.flatMap(_._2)).sortBy(availability => availabilityDate.toLocalDateTime(availability.start))
          sortedCombination.zip(sortedCombination.drop(1)).forall { case (a1, a2) => availabilityDate.toLocalDateTime(a1.end).isBefore(availabilityDate.toLocalDateTime(a2.start)) }
        }

        // Calculate the totalPreference for each valid combination
        val combinationPreferences = validCombinations.map { combination =>
          val totalPreference = combination.flatMap(_._2.map(_._1)).sum
          (totalPreference, combination.flatMap { case (viva, list) => list.map { case (_, availabilities) => (viva, availabilities) } })
        }

        // Get the combination of scheduled vivas with the maximum total preference
        combinationPreferences match
          case head :: tail =>
            tail.foldLeft(head) { (max, current) =>
              println("Current: " + current._1 + " Max: " + max._1)
              if (current._1 > max._1) current else max
            }
          case Nil => (0, List.empty[(Viva, List[Availability2])])
      case Left(error) => (0, List.empty[(Viva, List[Availability2])])


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
    val latestTimeSlot = starts.maxByOption(availabilityDate.toLocalDateTime)

    latestTimeSlot.map { modifiedLatestTimeSlot =>
      val start = modifiedLatestTimeSlot
      val end = modifiedLatestTimeSlot.plusTime(durationF)
      val preference = suitableAvailabilities.map(a => a.preference.to).sum

      (viva, (start, end, preference))
    }


  /**
   * This function finds the latest time slot with the highest preference. It does not consider the vivas.
   * @param suitableAvailabilities
   * @param duration
   * @return An optional tuple containing the start time, end time, and preference of the time slot.
   */
  def findLatestTimeSlotWithNoViva(suitableAvailabilities: List[Availability2], duration: agendaDuration): Option[List[(availabilityDate, availabilityDate, Int)]] =
    val durationF = parseDuration(duration)
    val starts = suitableAvailabilities.map(_.start)
    val ends = suitableAvailabilities.map(_.end)
    println("Starts: " + starts + " Ends: " + ends)
    val latestTimeSlot = starts.maxByOption(availabilityDate.toLocalDateTime)
    val earliestEndTime = ends.minByOption(availabilityDate.toLocalDateTime)
    println("Latest Time Slot: " + latestTimeSlot + " Earliest End Time: " + earliestEndTime)

    for {
      modifiedLatestTimeSlot <- latestTimeSlot
      end <- earliestEndTime
    } yield
      val start = modifiedLatestTimeSlot
      val preference = suitableAvailabilities.map(a => a.preference.to).sum
      generateTimeSlots(start, end, durationF, preference)



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
   * This function schedules the vivas with the maximum preference.
   * It returns the total preference and the list of vivas with their time slots.
   * @param duration
   * @param vivas
   * @return A tuple containing the total preference and the list of vivas with their time slots.
   */
  def scheduleMaxPreferenceVivas(duration: Result[VivaDuration], vivas: Result[List[Viva]]): (Int, List[(Viva, List[Availability2])]) =
    duration match
      case Right(durationF) =>
        scheduleVivas(duration, vivas) match
          case Right(agenda) =>
            val allTimeSlots = agenda.map { agendaItem =>
              val suitableAvailabilities = agendaItem.viva.roles.flatMap(_.availabilities)
              val numResources = agendaItem.viva.roles.length
              findAllTimeSlots(suitableAvailabilities, durationF.duration, numResources)
            }
            //println("All Time Slots: " + allTimeSlots)
            findGlobalPreference(vivas, allTimeSlots)
          case Left(error) =>
            (0, List.empty[(Viva, List[Availability2])])
      case Left(error) =>
        (0, List.empty[(Viva, List[Availability2])])


  /**
   * This function takes an XML Node, parses it to extract the necessary information,
   * schedules the vivas using the `scheduleMaxPreferenceVivas` function, and then
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
        val (totalPreference, vivaOutputs) = scheduleMaxPreferenceVivas(agendaResult, result)

        Right(
          <schedule xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../schedule.xsd"
                    totalPreference={totalPreference.toString}>
            {vivaOutputs.sortBy(vivaOutput => availabilityDate.toLocalDateTime(vivaOutput._2.head.start)).map { case (viva, availabilities) =>
            <viva student={viva.student.to} title={viva.title.to} start={dateFormatter(availabilityDate.toLocalDateTime(availabilities.head.start))} end={dateFormatter(availabilityDate.toLocalDateTime(availabilities.head.end))} preference={availabilities.map(_.preference.d).sum.toString}>
              {viva.roles.sortBy(_.role).flatMap(resource =>
              resource.role match {
                case Role2.President => Some(<president name={resource.name.to}/>)
                case Role2.Advisor => Some(<advisor name={resource.name.to}/>)
                case Role2.Coadvisor => Some(<coadvisor name={resource.name.to}/>)
                case Role2.Supervisor => Some(<supervisor name={resource.name.to}/>)
                case _ => None
              })}
            </viva>
          }}
          </schedule>
        )

