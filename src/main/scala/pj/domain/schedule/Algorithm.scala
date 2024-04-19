package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.DomaintoXml.{externalToXml, teacherToXml}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart}
import pj.domain.schedule.Utils.stringToDuration
import pj.domain.schedule.XMLtoDomain.{getAgendaDuration, getExternals, getPreference, getTeachers, getVivas}
import pj.io.FileIO

import java.time.format.DateTimeFormatter
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
  def intersectAvailabilities(viva: Viva, teachers: List[Resource], externals: List[Resource]): Result[List[Availability]] =
    // Get the ids of the resources required for the viva
    val requiredResourceIds = viva.roles.keys.toList

    // Get the availabilities of the required resources
    val requiredAvailabilities = teachers.filter(t => requiredResourceIds.contains(t.id)).flatMap(_.availabilities) ++
      externals.filter(e => requiredResourceIds.contains(e.id)).flatMap(_.availabilities)

    // Sort availabilities by start time
    val sortedAvailabilities = requiredAvailabilities.sortBy(_.start.to)

    // Find overlapping availabilities
    val overlappingAvailabilities = sortedAvailabilities.foldLeft[List[Availability]](Nil) { (acc, availability) =>
      acc match
        case Nil => List(availability)
        case head :: tail =>
          if (availability.start.to.isBefore(head.end.to) && availability.end.to.isAfter(head.start.to))
            // Overlapping availability found, create a new availability with the later start time and earlier end time
            Availability(
              if (availability.start.to.isAfter(head.start.to)) availability.start else head.start,
              if (availability.end.to.isBefore(head.end.to)) availability.end else head.end,
              head.preference // keep the preference of the first availability
            ) :: tail
          else
            // No overlap, add the availability to the list
            availability :: acc
    }

    // If there is no common availability slot, return an error
    if (overlappingAvailabilities.isEmpty)
      Left(DomainError.XMLError("No common availability slot found"))
    else
      Right(overlappingAvailabilities)

  /**
   * Function to find the earliest time slot for a viva.
   *
   * @param viva      The viva for which the earliest time slot is required.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return The earliest time slot as an Option.
   */
  def findEarliestTimeSlot(viva: Viva, teachers: List[Resource], externals: List[Resource]): Result[Option[Availability]] =
    intersectAvailabilities(viva, teachers, externals).flatMap { intersectedAvailabilities =>
      println(s"Intersected availabilities for viva ${viva.student}: $intersectedAvailabilities")

      val suitableAvailabilities = intersectedAvailabilities.filter(a => a.end.to.isAfter(a.start.to))
      println(s"Suitable availabilities for viva ${viva.student}: $suitableAvailabilities")

      implicit val availabilityStartOrdering: Ordering[Availability] = Ordering.by(_.start.to)
      val earliestTimeSlot = suitableAvailabilities.minByOption(identity)
      println(s"Earliest time slot for viva ${viva.student}: $earliestTimeSlot")

      Right(earliestTimeSlot)
    }

  /**
   * Function to schedule a viva.
   *
   * @param viva      The viva to be scheduled.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return Result containing the scheduled viva and updated lists of teachers and externals.
   */
  //Include duration and save to the read xml
  def scheduleViva(viva: Viva, teachers: List[Resource], externals: List[Resource], agendaDuration: agendaDuration): Result[(Viva, Availability, List[Resource], List[Resource])] =
    findEarliestTimeSlot(viva, teachers, externals).flatMap:
      case Some(timeSlot) =>
        stringToDuration(agendaDuration.to) match
          case Some(duration) =>
            println("parseEnd")
            val endTime = timeSlot.start.to.plus(duration)
            println("endtime" + endTime)

            val updatedTeachers = teachers.map { teacher =>
              if (viva.roles.keys.toList.contains(teacher.id))
                val updatedAvailabilities = teacher.availabilities.flatMap { availability =>
                  val beforeStart = availability.start.to.isBefore(timeSlot.start.to)
                  val afterEnd = availability.end.to.isAfter(endTime)
                  val withinStartEnd = availability.start.to.isBefore(endTime) && availability.end.to.isAfter(timeSlot.start.to)

                  (beforeStart, afterEnd, withinStartEnd) match
                    case (true, true, true) =>
                      availabilityEnd.from(timeSlot.start.to) match
                        case Right(start) =>
                          availabilityStart.from(timeSlot.end.to) match
                            case Right(end) =>
                              List(
                                Availability(availability.start, start, availability.preference),
                                Availability(end, availability.end, availability.preference)
                              )
                            case Left(_) => List(availability)
                        case Left(_) => List(availability)

                    case (true, false, true) =>
                      availabilityEnd.from(timeSlot.start.to) match
                        case Right(timeSlotStart) =>
                          List(Availability(availability.start, timeSlotStart, availability.preference))
                        case Left(_) => List(availability)
                    case (false, true, true) =>
                      availabilityStart.from(timeSlot.start.to) match
                        case Right(timeSlotStart) =>
                          List(Availability(timeSlotStart, availability.end, availability.preference))
                        case Left(_) => List(availability)
                    case _ => List(availability)
                }
                teacher.copy(availabilities = updatedAvailabilities)
              else
                teacher
            }
            println(s"Updated teachers: $updatedTeachers")

            val updatedExternals = externals.map { external =>
              if (viva.roles.keys.toList.contains(external.id))
                val updatedAvailabilities = external.availabilities.flatMap { availability =>
                  val beforeStart = availability.start.to.isBefore(timeSlot.start.to)
                  val afterEnd = availability.end.to.isAfter(timeSlot.end.to)
                  val withinStartEnd = availability.start.to.isBefore(timeSlot.end.to) && availability.end.to.isAfter(timeSlot.start.to)

                  (beforeStart, afterEnd, withinStartEnd) match
                    case (true, true, true) =>
                      availabilityEnd.from(timeSlot.start.to) match
                        case Right(start) =>
                          availabilityStart.from(timeSlot.end.to) match
                            case Right(end) =>
                              List(
                                Availability(availability.start, start, availability.preference),
                                Availability(end, availability.end, availability.preference)
                              )
                            case Left(_) => List(availability)
                        case Left(_) => List(availability)

                    case (true, false, true) =>
                      availabilityEnd.from(timeSlot.start.to) match
                        case Right(timeSlotStart) =>
                          List(Availability(availability.start, timeSlotStart, availability.preference))
                        case Left(_) => List(availability)
                    case (false, true, true) =>
                      availabilityStart.from(timeSlot.start.to) match
                        case Right(timeSlotStart) =>
                          List(Availability(timeSlotStart, availability.end, availability.preference))
                        case Left(_) => List(availability)
                    case _ => List(availability)
                }
                external.copy(availabilities = updatedAvailabilities)
              else
                external
            }
            println(s"Updated externals: $updatedExternals")

            println(s"Scheduled viva ${viva.student} at ${timeSlot.start} - ${timeSlot.end}")
            Right((viva, timeSlot, updatedTeachers, updatedExternals))
          case None =>
            Left(DomainError.XMLError("Failed to parse agenda duration"))
      case None =>
        Left(DomainError.XMLError("Failed to schedule viva"))

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
        (scheduledViva, timeSlot, updatedTeachers, updatedExternals) <- scheduleViva(viva, teachers, externals, agendaDuration)
      } yield ((scheduledViva, timeSlot) :: scheduledVivasList, updatedTeachers, updatedExternals, totalPreference)
    }

  /**
   * Function to generate XML output for scheduled vivas.
   *
   * @param scheduledVivas  List of scheduled vivas.
   * @param totalPreference Total preference.
   * @return XML element representing the schedule.
   */
  def scheduleVivas(xml: Node): Result[Elem] =
    for
      agendaDuration <- getAgendaDuration(xml)
      vivas <- getVivas(xml)
      teachers <- getTeachers(xml)
      externals <- getExternals(xml)
      preference <- getPreference(xml)
      scheduledVivas <- scheduleAllVivas(vivas, teachers, externals, agendaDuration)
      (scheduledVivasList, _, _, totalPreference) = scheduledVivas
    yield
      println(s"Scheduled vivas: $scheduledVivasList")
      val sortedScheduledVivasList = scheduledVivasList.sortBy(_._1.student.to)
      <schedule
      xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference={totalPreference.toString} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        {sortedScheduledVivasList.map { case (viva, availability) =>
        <viva student={viva.student.to} title={viva.title.to} start={availability.start.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} end={availability.end.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} preference={preference.to.toString}>
          {viva.roles.toList.sortBy(_._2).map { case (id, role) =>
          role match
            case Role.President => <president name={id.to}/>
            case Role.Advisor => <advisor name={id.to}/>
            case Role.Supervisor => <supervisor name={id.to}/>
            case Role.Coadvisor => <coadvisor name={id.to}/>
        }}
        </viva>
      }}
      </schedule>

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
      teachersXml = updatedTeachers.map(teacherToXml)
      externalsXml = updatedExternals.map(externalToXml)
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
                case Role.Supervisor => <supervisor id={id.to}/>
                case Role.Coadvisor => <coadvisor id={id.to}/>
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


