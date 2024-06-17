package pj.domain.schedule

import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate, resourceId}
import pj.domain.schedule.Domain.{Availability2, Role2, Viva}

import java.time.format.DateTimeFormatter
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


  def dateFormatter(date: LocalDateTime): String =
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")
    date.format(formatter)


  // Function to get the role of a resource from a Viva object
  def getRoleFromViva(viva: Viva, resourceId: resourceId): Role2 =
    viva.roles.find(_.id == resourceId).map(_.role).getOrElse(Role2.None)


  // Check if two scheduled vivas have overlapping availabilities
  def overlaps(slot1: (Viva, List[(Int, List[Availability2])]), slot2: (Viva, List[(Int, List[Availability2])])): Boolean =
    val (_, list1) = slot1
    val (_, list2) = slot2
    list1.flatMap(_._2).exists(a1 => list2.flatMap(_._2).exists(a2 => !(availabilityDate.toLocalDateTime(a1.end).isBefore(availabilityDate.toLocalDateTime(a2.start)) || availabilityDate.toLocalDateTime(a2.end).isBefore(availabilityDate.toLocalDateTime(a1.start)))))

  def anyOverlap(slots: List[(Viva, List[(Int, List[Availability2])])]): Boolean =
    slots.combinations(2).exists { case List(slot1, slot2) =>
      overlaps(slot1, slot2)
    }


  // Function to generate a combination of a possible schedule with a corresponding viva
  def sequence[A](list: List[List[A]]): List[List[A]] = list match
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- sequence(t)) yield xh :: xt


  /**
   * This function generates a list of time slots based on the availability of a resource.
   *
   * @param start      The start of the availability.
   * @param end        The end of the availability.
   * @param duration   The duration of each time slot.
   * @param preference The preference of the time slot.
   * @return A list of tuples representing the time slots.
   */
  def generateTimeSlots(start: availabilityDate, end: availabilityDate, duration: Duration, preference: Int): List[(availabilityDate, availabilityDate, Int)] =
    val startDateTime = availabilityDate.toLocalDateTime(start)
    val endDateTime = availabilityDate.toLocalDateTime(end)

    if (startDateTime.plus(duration).isAfter(endDateTime))
      List.empty[(availabilityDate, availabilityDate, Int)]
    else
      (start, start.plusTime(duration), preference) :: generateTimeSlots(start.plusTime(duration), end, duration, preference)


  /**
   * This function adjusts the availability of a resource based on overlaps with a given time interval.
   *
   * @param availability The original availability of the resource.
   * @param overlapStart The start of the overlapping time interval.
   * @param overlapEnd   The end of the overlapping time interval.
   * @return A list of Availability instances representing the adjusted availability of the resource.
   */
  def fixAvailabilities(availability: Availability2, overlapStart: availabilityDate, overlapEnd: availabilityDate): List[Availability2] =
    val overlapStartDateTime = overlapStart.to
    val overlapEndDateTime = overlapEnd.to
    val availabilityStartDateTime = availability.start.to
    val availabilityEndDateTime = availability.end.to

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

