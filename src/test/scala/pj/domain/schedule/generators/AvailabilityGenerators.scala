package pj.domain.schedule.generators

import org.scalacheck.Gen
import pj.domain.schedule.Domain.Availability
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityEnd, availabilityStart}

import java.time.{LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter

object AvailabilityGenerators:

  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd:HH:mm:ss")
  val timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")



  def generateDuration: Gen[agendaDuration] =
    //Generate a duration of 1, 1:30 or 2 hours
    val duration = Gen.oneOf("01:00:00", "01:30:00", "02:00:00")
    for {
      d <- duration
      duration = agendaDuration.from(d)
      agendaDuration <- duration.fold(_ => Gen.fail, duration => Gen.const(duration))
    } yield agendaDuration


  val commonDate: LocalDateTime = LocalDateTime.of(2024, 5, 15, 12, 0) // Set a common date


  def generateAvailabilityStart: Gen[availabilityStart] =
    for {
      startDateTime <- Gen.chooseNum(-5, 5).map(hours => commonDate.plusHours(hours)) // Generate start dates around the common date
      dateStr = startDateTime.format(dateFormatter)
      start = availabilityStart.from(LocalDateTime.parse(dateStr, dateFormatter))
      availabilityStart <- start.fold(_ => Gen.fail, start => Gen.const(start))
    } yield availabilityStart

  def generateAvailabilityEnd(duration: agendaDuration, start: availabilityStart): Gen[availabilityEnd] =
    val durationTime = LocalTime.parse(duration.to, timeFormatter)

    val durationHours = durationTime.getHour
    val durationMinutes = durationTime.getMinute
    val durationSeconds = durationTime.getSecond
    val endDateTimeWithDuration = start.to.plusHours(durationHours).plusMinutes(durationMinutes).plusSeconds(durationSeconds)
    
    for {
      endDateTime <- Gen.chooseNum(0, 5).map(hours => endDateTimeWithDuration.plusHours(hours)) // Generate end dates around the start date
      dateStr = endDateTime.format(dateFormatter)
      end = availabilityEnd.from(LocalDateTime.parse(dateStr, dateFormatter))
      availabilityEnd <- end.fold(_ => Gen.fail, end => Gen.const(end))
    } yield availabilityEnd

  
  def availabilityStartBeforeEnd(start: List[availabilityStart], end: List[availabilityEnd]): Boolean =
    start.zip(end).forall:
      case (s, e) => s.to.isBefore(e.to)

  
  def generatePreference: Gen[Preference] =
    Gen.chooseNum(1, 5).map(n => Preference.from(n.toString).getOrElse(Preference(1)))

  
  def generateAvailability(duration: agendaDuration): Gen[Availability] = for {
    start <- generateAvailabilityStart
    end <- generateAvailabilityEnd(duration, start)
    preference <- generatePreference
  } yield Availability(start, end, preference)


