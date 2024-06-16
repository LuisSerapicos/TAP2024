package pj.domain.schedule

import pj.domain.schedule.Domain.{Availability2, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration}
import pj.domain.schedule.Utils.stringToDuration

import java.time.Duration
import java.time.format.DateTimeFormatter
import scala.xml.Elem

object DomaintoXml:

  /**
   * Extracts the vivas from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[List[Viva]] which is a Right[List[Viva]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def teacherToXml(teacher: Resource, agendaDuration: agendaDuration): Elem =
    <teacher id={teacher.id.to} name={teacher.name.to}>
      {teacher.availabilities.flatMap(availability => availabilityToXml(availability, agendaDuration))}
    </teacher>

  /**
   * Converts an external resource to an XML element.
   *
   * @param external       The external resource.
   * @param agendaDuration The duration of the agenda.
   * @return An Elem representing the external resource.
   */
  def externalToXml(external: Resource, agendaDuration: agendaDuration): Elem =
    <external id={external.id.to} name={external.name.to}>
      {external.availabilities.flatMap(availability => availabilityToXml(availability, agendaDuration))}
    </external>

  /**
   * Converts an availability to an XML element.
   *
   * @param availability   The availability.
   * @param agendaDuration The duration of the agenda.
   * @return An Option[Elem] which is a Some[Elem] if the availability duration is greater than or equal to the agenda duration, or a None otherwise.
   */
  def availabilityToXml(availability: Availability2, agendaDuration: agendaDuration): Option[Elem] =
    val duration = stringToDuration(agendaDuration.to).getOrElse(Duration.ZERO)
    val availabilityDuration = Duration.between(availability.start.to, availability.end.to)
    if (availabilityDuration.compareTo(duration) >= 0)
      Some(<availability start={availability.start.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} end={availability.end.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} preference={availability.preference.d.toString}/>)
    else
      None
