package pj.domain.schedule

import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration}
import pj.domain.schedule.Utils.stringToDuration

import java.time.Duration
import java.time.format.DateTimeFormatter
import scala.xml.Elem

object DomaintoXml:
  def teacherToXml(teacher: Resource, agendaDuration: agendaDuration): Elem =
    <teacher id={teacher.id.to} name={teacher.name.to}>
      {teacher.availabilities.flatMap(availability => availabilityToXml(availability, agendaDuration))}
    </teacher>

  def externalToXml(external: Resource, agendaDuration: agendaDuration): Elem =
    <external id={external.id.to} name={external.name.to}>
      {external.availabilities.flatMap(availability => availabilityToXml(availability, agendaDuration))}
    </external>

  def availabilityToXml(availability: Availability, agendaDuration: agendaDuration): Option[Elem] =
    val duration = stringToDuration(agendaDuration.to).getOrElse(Duration.ZERO)
    val availabilityDuration = Duration.between(availability.start.to, availability.end.to)
    if (availabilityDuration.compareTo(duration) >= 0)
      Some(<availability start={availability.start.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} end={availability.end.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} preference={availability.preference.d.toString}/>)
    else
      None
