package pj.domain.schedule.properties

import org.scalacheck.*
import pj.domain.schedule.Domain.Availability
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{availabilityEnd, availabilityStart}

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}

object AvailabilityProperties extends Properties("AvailabilityProperties"):

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)


  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd:HH:mm:ss")

  def generateAvailabilityStart: Gen[availabilityStart] =
    for {
      year <- Gen.chooseNum(1900, 2100)
      month <- Gen.chooseNum(1, 12)
      day <- Gen.chooseNum(1, 28)
      hour <- Gen.chooseNum(0, 23)
      minute <- Gen.chooseNum(0, 59)
      second <- Gen.chooseNum(0, 59)
      dateStr = f"$year-$month%02d-$day%02d:$hour%02d:$minute%02d:$second%02d"
      start = availabilityStart.from(LocalDateTime.parse(dateStr, formatter))
      availabilityStart <- start.fold(_ => Gen.fail, start => Gen.const(start))
    } yield availabilityStart

  def generateAvailabilityEnd(start: availabilityStart): Gen[availabilityEnd] =
    for {
      durationHours <- Gen.chooseNum(1, 24)
      durationMinutes <- Gen.chooseNum(0, 59)
      durationSeconds <- Gen.chooseNum(0, 59)
      endDateTime = start.to.plusHours(durationHours).plusMinutes(durationMinutes).plusSeconds(durationSeconds)
      dateStr = f"${endDateTime.getYear}-${endDateTime.getMonthValue}%02d-${endDateTime.getDayOfMonth}%02d:${endDateTime.getHour}%02d:${endDateTime.getMinute}%02d:${endDateTime.getSecond}%02d"
      end = availabilityEnd.from(LocalDateTime.parse(dateStr, formatter))
      availabilityEnd <- end.fold(_ => Gen.fail, end => Gen.const(end))
    } yield availabilityEnd

  def availabilityStartBeforeEnd(start: List[availabilityStart], end: List[availabilityEnd]): Boolean =
    start.zip(end).forall:
      case (s, e) => s.to.isBefore(e.to)

  def generatePreference: Gen[Preference] =
    Gen.chooseNum(1, 5).map(n => Preference.from(n.toString).getOrElse(Preference(1)))


  def generateAvailability: Gen[Availability] = for {
    start <- generateAvailabilityStart
    end <- generateAvailabilityEnd(start)
    preference <- generatePreference
  } yield Availability(start, end, preference)


  property("Generate Preference") = Prop.forAll(generatePreference) { preference =>
    preference.d >= 1 && preference.d <= 5
  }

  property("Generate Availability") = Prop.forAll(generateAvailability) { (availability: Availability) =>
    availability.start.to.toString.nonEmpty && availability.end.to.toString.nonEmpty && availability.preference != null
  }

  property("Availability start before end") = Prop.forAll(Gen.listOfN(1000, generateAvailability)) { availabilities =>
    availabilities.forall(a => a.start.to.isBefore(a.end.to))
  }

