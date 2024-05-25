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
   * Function to intersect availabilities of resources required for a viva.
   *
   * @param viva      The viva for which resources are required.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return List of intersected availabilities.
   */
  def intersectAvailabilities(viva: Viva, teachers: List[Resource], externals: List[Resource], agendaDuration: agendaDuration): Result[List[Availability]] =
    // Get the ids of the resources required for the viva
    val requiredResourceIds = viva.roles.keys.toList


    // Get the required resources
    val requiredResources = teachers.filter(t => requiredResourceIds.contains(t.id)) ++
      externals.filter(e => requiredResourceIds.contains(e.id))

    // Create a map of resource IDs to their availabilities for only the required resources
    val availabilitiesByResource = requiredResources.map(resource => resource.id -> resource.availabilities).toMap

    // Function to check if an availability is common to all resources
    def isCommonAvailability(availability: Availability): Boolean =
      availabilitiesByResource.forall { case (_, availabilities) =>
        availabilities.exists(a => a.start.to.isBefore(availability.end.to) && a.end.to.isAfter(availability.start.to))
      }

    // Find common availabilities
    val commonAvailabilities = requiredResources.flatMap(_.availabilities).filter(isCommonAvailability)

    // If there is no common availability slot, return an error
    if (commonAvailabilities.isEmpty)
      Left(DomainError.XMLError("No common availability slot found"))
    else
      val overlappingAvailabilities = for {
        a1 <- commonAvailabilities
        a2 <- commonAvailabilities
        if a1 != a2 && (a1.start.to.isBefore(a2.end.to) || a1.start.to.isEqual(a2.end.to)) && (a1.end.to.isAfter(a2.start.to) || a1.end.to.isEqual(a2.start.to))
        newAvailability = Availability(
          if (a1.start.to.isAfter(a2.start.to)) a1.start else a2.start,
          if (a1.end.to.isBefore(a2.end.to)) a1.end else a2.end,
          a1.preference + a2.preference // sum up the preferences of the overlapping availabilities
        )
      } yield
        newAvailability

      Right(overlappingAvailabilities)

  /**
   * Function to find the earliest time slot for a viva.
   *
   * @param viva      The viva for which the earliest time slot is required.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return The earliest time slot as an Option.
   */
  def findEarliestTimeSlot(viva: Viva, teachers: List[Resource], externals: List[Resource], agendaDuration: agendaDuration): Result[Option[Availability]] =
    intersectAvailabilities(viva, teachers, externals, agendaDuration).flatMap { intersectedAvailabilities =>

      val duration = stringToDuration(agendaDuration.to).getOrElse(Duration.ZERO)

      val suitableAvailabilities = intersectedAvailabilities.filter(a => a.end.to.isAfter(a.start.to.plus(duration.toMillis, ChronoUnit.MILLIS)) || a.end.to.isEqual(a.start.to.plus(duration.toMillis, ChronoUnit.MILLIS)))

      implicit val availabilityStartOrdering: Ordering[Availability] = Ordering.by(_.start.to)
      val earliestTimeSlot = suitableAvailabilities.minByOption(identity)

      // Modify the end time of the earliest time slot to be start time + duration
      val modifiedEarliestTimeSlot = earliestTimeSlot.map(a => a.copy(end = availabilityEnd.from(a.start.to.plus(duration.toMillis, ChronoUnit.MILLIS)).getOrElse(a.end)))

      Right(modifiedEarliestTimeSlot)
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
