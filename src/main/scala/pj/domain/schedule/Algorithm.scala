package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.DomaintoXml.{externalToXml, teacherToXml}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart}
import pj.domain.schedule.Utils.stringToDuration
import pj.domain.schedule.XMLtoDomain.{getAgendaDuration, getExternals, getPreference, getTeachers, getVivas, xml2}
import pj.io.FileIO

import java.time.Duration
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
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
  def intersectAvailabilities(currentResource: Resource, otherResources: List[Resource], duration: agendaDuration): Option[Availability2] =
    val durationF = parseDuration(duration)
    val a1 = currentResource.availabilities.sortBy(availability => availabilityDate.toLocalDateTime(availability.start))
    val a2 = otherResources.map(resource => resource.availabilities.sortBy(availability => availabilityDate.toLocalDateTime(availability.start)))

    a1.find { availability =>
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

    if (suitableAvailabilities.length != viva.roles.length) Left(DomainError.ImpossibleSchedule)
    else Right((viva, suitableAvailabilities))


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
      println("Overlap: " + start + " - " + end + " Preference: " + preference)

      (viva, (start, end, preference))
    }

  /**
   * Function to schedule a viva.
   *
   * @param viva      The viva to be scheduled.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return Result containing the scheduled viva and updated lists of teachers and externals.
   */
  def scheduleViva(viva: Viva, teachers: List[Resource], externals: List[Resource], agendaDuration: agendaDuration): Result[(Viva, Availability, List[Resource], List[Resource], Int)] =
    findEarliestTimeSlot(viva, teachers, externals, agendaDuration).flatMap:
      case Some(timeSlot) =>
        stringToDuration(agendaDuration.to) match
          case Some(duration) =>
            val endTime = timeSlot.start.to.plus(duration)
            availabilityStart.from(endTime) match
              case Right(finalEndTime) =>
                val updatedTeachers = teachers.map { teacher =>
                  if (viva.roles.keys.toList.contains(teacher.id))
                    val updatedAvailabilities = teacher.availabilities.flatMap { availability =>
                      val beforeStart = availability.start.to.isBefore(timeSlot.start.to)
                      val afterEnd = availability.end.to.isAfter(finalEndTime.to)
                      val withinStartEnd = availability.start.to.isBefore(finalEndTime.to) && availability.end.to.isAfter(timeSlot.start.to)
                      val availabilityDuration = Duration.between(availability.start.to, availability.end.to).compareTo(duration) >= 0

                      (beforeStart, afterEnd, withinStartEnd, availabilityDuration) match
                        case (true, true, true, true) =>
                          availabilityEnd.from(timeSlot.start.to) match
                            case Right(start) =>
                              availabilityStart.from(finalEndTime.to) match
                                case Right(end) =>
                                  List(
                                    Availability(availability.start, start, availability.preference),
                                    Availability(end, availability.end, availability.preference)
                                  ).filter(_ => availabilityDuration)
                                case Left(_) => List(availability)
                            case Left(_) => List(availability)

                        case (true, false, true, true) =>
                          availabilityEnd.from(timeSlot.start.to) match
                            case Right(timeSlotStart) =>
                              List(Availability(availability.start, timeSlotStart, availability.preference)).filter(_ => availabilityDuration)
                            case Left(_) => List(availability)

                        case (false, true, true, true) =>
                          availabilityStart.from(finalEndTime.to) match
                            case Right(timeSlotEnd) =>
                              List(Availability(timeSlotEnd, availability.end, availability.preference)).filter(_ => availabilityDuration)
                            case Left(_) => List(availability)

                        case (_, _, _, true) => List(availability)

                        case (_, _, _, false) => Nil
                    }
                    teacher.copy(availabilities = updatedAvailabilities)
                  else
                    teacher
                }

                val updatedExternals = externals.map { external =>
                  if (viva.roles.keys.toList.contains(external.id))
                    val updatedAvailabilities = external.availabilities.flatMap { availability =>
                      val beforeStart = availability.start.to.isBefore(timeSlot.start.to)
                      val afterEnd = availability.end.to.isAfter(finalEndTime.to)
                      val withinStartEnd = availability.start.to.isBefore(finalEndTime.to) && availability.end.to.isAfter(timeSlot.start.to)
                      val availabilityDuration = Duration.between(availability.start.to, availability.end.to).compareTo(duration) >= 0

                      (beforeStart, afterEnd, withinStartEnd, availabilityDuration) match
                        case (true, true, true, true) =>
                          availabilityEnd.from(timeSlot.start.to) match
                            case Right(start) =>
                              availabilityStart.from(finalEndTime.to) match
                                case Right(end) =>
                                  List(
                                    Availability(availability.start, start, availability.preference),
                                    Availability(end, availability.end, availability.preference)
                                  ).filter(_ => availabilityDuration)
                                case Left(_) => List(availability)
                            case Left(_) => List(availability)

                        case (true, false, true, true) =>
                          availabilityEnd.from(timeSlot.start.to) match
                            case Right(timeSlotStart) =>
                              List(Availability(availability.start, timeSlotStart, availability.preference)).filter(_ => availabilityDuration)
                            case Left(_) => List(availability)

                        case (false, true, true, true) =>
                          availabilityStart.from(finalEndTime.to) match
                            case Right(timeSlotEnd) =>
                              List(Availability(timeSlotEnd, availability.end, availability.preference)).filter(_ => availabilityDuration)
                            case Left(_) => List(availability)

                        case (_, _, _, true) => List(availability)

                        case (_, _, _, false) => Nil
                    }
                    external.copy(availabilities = updatedAvailabilities)
                  else
                    external
                }


                val vivaPreference = timeSlot.preference.d // get the preference of the viva
                Right((viva, timeSlot, updatedTeachers, updatedExternals, vivaPreference))
              case Left(_) =>
                Left(DomainError.XMLError("Failed to convert end time to availabilityStart"))
          case None =>
            Left(DomainError.XMLError("Failed to parse agenda duration"))
      case None =>
        Left(DomainError.ImpossibleSchedule)

  /**
   * Function to schedule all vivas.
   *
   * @param vivas     List of vivas to be scheduled.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return Result containing the list of scheduled vivas and total preference.
   */
  def scheduleAllVivas(vivas: List[Viva], teachers: List[Resource], externals: List[Resource], agendaDuration: agendaDuration): Result[(List[(Viva, Availability)], List[Resource], List[Resource], Int)] =
    vivas.foldLeft[Result[(List[(Viva, Availability)], List[Resource], List[Resource], Int)]](Right((List.empty[(Viva, Availability)], teachers, externals, 0))) { case (acc, viva) =>
      for {
        (scheduledVivasList, teachers, externals, totalPreference) <- acc
        (scheduledViva, timeSlot, updatedTeachers, updatedExternals, vivaPreference) <- scheduleViva(viva, teachers, externals, agendaDuration)
      } yield ((scheduledViva, timeSlot) :: scheduledVivasList, updatedTeachers, updatedExternals, totalPreference + vivaPreference)
    }

  /**
   * Function to generate XML output for scheduled vivas.
   *
   * @param scheduledVivas  List of scheduled vivas.
   * @param totalPreference Total preference.
   * @return XML element representing the schedule.
   */
  def scheduleVivas(xml: Node): Result[Elem] =
    getAgendaDuration(xml) match
      case Left(error) => Left(DomainError.XMLError(s"Error getting agenda duration: ${error}"))
      case Right(agendaDuration) =>
        getVivas(xml) match
          case Left(error) => Left(DomainError.XMLError(s"Error getting vivas: ${error}"))
          case Right(vivas) =>
            getTeachers(xml) match
              case Left(error) => Left(DomainError.XMLError(s"Error getting teachers: ${error}"))
              case Right(teachers) =>
                getExternals(xml) match
                  case Left(error) => Left(error)
                  case Right(externals) =>
                    getPreference(xml) match
                      case Left(error) => Left(DomainError.XMLError(s"Error getting preference: ${error}"))
                      case Right(preference) =>
                        scheduleAllVivas(vivas, teachers, externals, agendaDuration) match
                          case Left(error) => Left(error)
                          case Right((scheduledVivasList, _, _, totalPreference)) =>
                            val sortedScheduledVivasList = scheduledVivasList.sortBy(_._2.start.to)
                            Right(
                              <schedule xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference={totalPreference.toString} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                                {sortedScheduledVivasList.map { case (viva, availability) =>
                                <viva student={viva.student.to} title={viva.title.to} start={availability.start.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} end={availability.end.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} preference={availability.preference.d.toString}>
                                  {viva.roles.toList.sortBy(_._2).map { case (id, role) =>
                                  role match {
                                    case Role.President => <president name={id.to}/>
                                    case Role.Advisor => <advisor name={id.to}/>
                                    case Role.Coadvisor => <coadvisor name={id.to}/>
                                    case Role.Supervisor => <supervisor name={id.to}/>
                                  }
                                }}
                                </viva>
                              }}
                              </schedule>
                            )

  /**
   * This function updates the agenda by scheduling all vivas and saving the updated agenda to an XML file.
   *
   * @param xml              The XML node representing the input data.
   * @param originalFilePath The file path where the updated XML will be saved.
   * @return A Result[Unit] which is a Right[Unit] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def updateAgenda(xml: Node, originalFilePath: String): Result[Unit] =
    for {
      teachers <- getTeachers(xml)
      externals <- getExternals(xml)
      vivas <- getVivas(xml)
      duration <- getAgendaDuration(xml)
      scheduledVivas <- scheduleAllVivas(vivas, teachers, externals, duration)
      (scheduledVivasList, updatedTeachers, updatedExternals, totalPreference) = scheduledVivas
      teachersXml = updatedTeachers.map(teacher => teacherToXml(teacher, duration))
      externalsXml = updatedExternals.map(external => externalToXml(external, duration))
    } yield
      val sortedScheduledVivasList = scheduledVivasList.sortBy(_._1.student.to)
      val agendaXml =
        <agenda xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../agenda.xsd" duration={duration.to}>
          <vivas>
            {sortedScheduledVivasList.map { case (viva, availability) =>
            <viva student={viva.student.to} title={viva.title.to}>
              {viva.roles.toList.sortBy(_._2).map { case (id, role) =>
              role match
                case Role.President => <president id={id.to}/>
                case Role.Advisor => <advisor id={id.to}/>
                case Role.Coadvisor => <coadvisor id={id.to}/>
                case Role.Supervisor => <supervisor id={id.to}/>
            }}
            </viva>
          }}
          </vivas>
          <resources>
            <teachers>
              {teachersXml}
            </teachers>
            <externals>
              {externalsXml}
            </externals>
          </resources>
        </agenda>
      FileIO.save(originalFilePath, agendaXml)


  val schedule = xml2.flatMap(scheduleVivas)
  val scheduleResult = xml2.flatMap(scheduleVivas)

  val xmlNode: xml.Node = xml2 match
    case Right(xmlElem) => xmlElem
    case Left(error) => <error>
      {error.toString}
    </error>
  val agendaUpdate = updateAgenda(xmlNode, "C:\\Users\\Luis Serapicos\\Desktop\\1230196_1231304_ncf\\files\\test\\ms01\\simple01_updated.xml")


  scheduleResult match
    case Right(scheduleElem) =>
      FileIO.save("C:\\Users\\Luis Serapicos\\Desktop\\1230196_1231304_ncf\\files\\test\\ms01\\simple01_testOut.xml", scheduleElem)
    case Left(error) =>
      val errorElem = <error xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../scheduleError.xsd" message={s"${error}"}/>
      FileIO.save("C:\\Users\\Luis Serapicos\\Desktop\\1230196_1231304_ncf\\files\\test\\ms01\\simple01_testOutError.xml", errorElem)
