package pj.domain.schedule

import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate, resourceId}
import pj.domain.schedule.newDomain.{Availability2, Role2, Viva}

import java.time.{Duration, LocalDateTime}
import scala.util.Try

object Utils:
  /**
   * Converts a string representation of a duration to a Duration object.
   *
   * The string should be in the format "HH:MM:SS", where HH represents hours, MM represents minutes, and SS represents seconds.
   *
   * @param durationString The string representation of the duration.
   * @return An Option[Duration] which is a Some[Duration] if the conversion is successful, or a None if an error occurs during the conversion.
   */
  def stringToDuration(durationString: String): Option[Duration] =
    Try(Duration.ofHours(durationString.split(":")(0).toLong)
      .plusMinutes(durationString.split(":")(1).toLong)
      .plusSeconds(durationString.split(":")(2).toLong)
    ).toOption


  def parseDuration(duration: agendaDuration): Duration =
    stringToDuration(duration.to).getOrElse(Duration.ZERO)


  def parseDate(dateString: String): LocalDateTime = LocalDateTime.parse(dateString)


  // Function to get the role of a resource from a Viva object
  def getRoleFromViva(viva: Viva, resourceId: resourceId): Role2 =
    viva.roles.find(_.id == resourceId).map(_.role).getOrElse(Role2.None)


  /**
   * This function adjusts the availability of a resource based on overlaps with a given time interval.
   *
   * @param availability The original availability of the resource.
   * @param overlapStart The start of the overlapping time interval.
   * @param overlapEnd   The end of the overlapping time interval.
   * @return A list of Availability instances representing the adjusted availability of the resource.
   */
  def fixAvailabilities(availability: Availability2, overlapStart: availabilityDate, overlapEnd: availabilityDate): List[Availability2] =
    val overlapStartDateTime = overlapStart.toLocalDateTime2
    val overlapEndDateTime = overlapEnd.toLocalDateTime2
    val availabilityStartDateTime = availability.start.toLocalDateTime2
    val availabilityEndDateTime = availability.end.toLocalDateTime2

    val isBefore = overlapEndDateTime.isBefore(availabilityEndDateTime)
    val isAfter = overlapStartDateTime.isAfter(availabilityStartDateTime)
    val isEqualStart = overlapStartDateTime.isEqual(availabilityStartDateTime)
    val isEqualEnd = overlapEndDateTime.isEqual(availabilityEndDateTime)

    (isBefore, isAfter, isEqualStart, isEqualEnd) match
      case (true, true, false, false) =>
        // Middle of the availability, split it into two
        val beforeOverlap = Availability2(availability.start, availabilityDate.from(overlapStartDateTime), availability.preference)
        val afterOverlap = Availability2(availabilityDate.from(overlapEndDateTime), availability.end, availability.preference)
        List(beforeOverlap, afterOverlap)
      case (true, false, true, false) =>
        // Start of the availability
        val afterOverlap = Availability2(availabilityDate.from(overlapEndDateTime), availability.end, availability.preference)
        List(afterOverlap)
      case (false, true, false, true) =>
        // End of the availability
        val beforeOverlap = Availability2(availability.start, availabilityDate.from(overlapStartDateTime), availability.preference)
        List(beforeOverlap)
      case _ =>
        // Remove if the overlap is the same as the availability
        List()

