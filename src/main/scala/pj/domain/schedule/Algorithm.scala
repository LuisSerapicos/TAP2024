package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, availabilityEnd, availabilityStart}
import pj.domain.schedule.XMLtoDomain.{getAgendaDuration, getExternals, getPreference, getTeachers, getVivas}

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
  def intersectAvailabilities(viva: Viva, teachers: List[Resource], externals: List[Resource]): List[Availability] =
    // Get the ids of the resources required for the viva
    val requiredResourceIds = viva.roles.keys.toList

    // Get the availabilities of the required resources
    val requiredAvailabilities = teachers.filter(t => requiredResourceIds.contains(t.id)).flatMap(_.availabilities) ++
      externals.filter(e => requiredResourceIds.contains(e.id)).flatMap(_.availabilities)

    // Find the intersection of all these availabilities
    val intersection = requiredAvailabilities.groupBy(a => (a.start, a.end)).filter(_._2.sizeIs == requiredResourceIds.sizeIs).values.flatten.toList

    // Return the intersection of these availabilities
    intersection

  /**
   * Function to find the earliest time slot for a viva.
   *
   * @param viva      The viva for which the earliest time slot is required.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return The earliest time slot as an Option.
   */
  def findEarliestTimeSlot(viva: Viva, teachers: List[Resource], externals: List[Resource]): Option[availabilityStart] =
    // Get the intersected availabilities of all resources required for the viva
    val intersectedAvailabilities = intersectAvailabilities(viva, teachers, externals)

    implicit val availabilityStartOrdering: Ordering[availabilityStart] = Ordering.by(_.to)
    // Find the earliest time slot from these intersected availabilities
    val earliestTimeSlot = intersectedAvailabilities.minByOption(_.start).map(_.start)

    // Return the earliest time slot
    earliestTimeSlot

  /**
   * Function to schedule a viva.
   *
   * @param viva      The viva to be scheduled.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return Result containing the scheduled viva and updated lists of teachers and externals.
   */
  def scheduleViva(viva: Viva, teachers: List[Resource], externals: List[Resource]): Result[(Viva, List[Resource], List[Resource])] =
    // Find the earliest time slot for the viva
    val earliestTimeSlot = findEarliestTimeSlot(viva, teachers, externals)

    earliestTimeSlot match
      case Some(timeSlot) =>
        // Update the availabilities of the resources involved in the viva
        val updatedTeachers = teachers.map { teacher =>
          if (viva.roles.keys.toList.contains(teacher.id))
            val updatedAvailabilities = teacher.availabilities.filterNot(a => a.start == timeSlot)
            teacher.copy(availabilities = updatedAvailabilities)
          else
            teacher
        }

        val updatedExternals = externals.map { external =>
          if (viva.roles.keys.toList.contains(external.id))
            val updatedAvailabilities = external.availabilities.filterNot(a => a.start == timeSlot)
            external.copy(availabilities = updatedAvailabilities)
          else
            external
        }

        Right((viva, updatedTeachers, updatedExternals))

      case None =>
        // If no time slot was found, return an error
        Left(DomainError.XMLError("Failed to schedule viva"))

  /**
   * Function to schedule all vivas.
   *
   * @param vivas     List of vivas to be scheduled.
   * @param teachers  List of teachers.
   * @param externals List of externals.
   * @return Result containing the list of scheduled vivas and total preference.
   */
  def scheduleAllVivas(vivas: List[Viva], teachers: List[Resource], externals: List[Resource]): Result[(List[Viva], Int)] =
    vivas.foldLeft[Result[(List[Viva], Int)]](Right((List.empty[Viva], 0))) { case (acc, viva) =>
      acc.flatMap { case (scheduledVivas, totalPreference) =>
        scheduleViva(viva, teachers, externals).map { case (scheduledViva, updatedTeachers, updatedExternals) =>
          (scheduledViva :: scheduledVivas, totalPreference)
        }
      }
    }

  /**
   * Function to generate XML output for scheduled vivas.
   *
   * @param scheduledVivas  List of scheduled vivas.
   * @param totalPreference Total preference.
   * @return XML element representing the schedule.
   */
  def generateXMLOutput(scheduledVivas: List[Viva], totalPreference: Int): Elem =
    <schedule xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference={totalPreference.toString} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      {scheduledVivas.map { viva =>
      <viva student={viva.student.to} title={viva.title.to} start="2024-05-28T09:00:00" end="2024-05-28T10:30:00" preference="4">
        {viva.roles.map { case (id, role) =>
        role match
          case Role.President => <president name={id.to}/>
          case Role.Advisor => <advisor name={id.to}/>
          case Role.Coadvisor => <coadvisor name={id.to}/>
          case Role.Supervisor => <supervisor name={id.to}/>
      }}
      </viva>
    }}
    </schedule>

  /**
   * Function to schedule vivas and generate XML output.
   *
   * @param xml XML node representing the input data.
   * @return Result containing the XML element representing the schedule.
   */
  def scheduleVivas(xml: Node): Result[Elem] =
    for
      agendaDuration <- getAgendaDuration(xml)
      vivas <- getVivas(xml)
      teachers <- getTeachers(xml)
      externals <- getExternals(xml)
      preference <- getPreference(xml)
      scheduledVivas <- scheduleAllVivas(vivas, teachers, externals)
      (scheduledVivasList, totalPreference) = scheduledVivas
    yield
      <schedule
      xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference={totalPreference.toString} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        {scheduledVivasList.map { viva =>
        <viva student={viva.student.to} title={viva.title.to} start={availabilityStart.toString} end={availabilityEnd.toString} preference={preference.to.toString}>
          {viva.roles.toList.sortBy(_._2).map { case (id, role) =>
          role match
            case Role.President => <president name={id.to}/>
            case Role.Advisor => <advisor name={id.to}/>
            case Role.Coadvisor => <coadvisor name={id.to}/>
            case Role.Supervisor => <supervisor name={id.to}/>
        }}
        </viva>
      }}
      </schedule>
