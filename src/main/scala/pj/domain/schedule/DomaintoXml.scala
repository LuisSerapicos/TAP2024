package pj.domain.schedule

import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.Role

import java.time.format.DateTimeFormatter
import scala.xml.Elem

object DomaintoXml:
  def teacherToXml(teacher: Resource): Elem =
    <teacher id={teacher.id.to} name={teacher.name.to}>
      {teacher.availabilities.map(availabilityToXml)}
    </teacher>

  def externalToXml(external: Resource): Elem =
    <external id={external.id.to} name={external.name.to}>
      {external.availabilities.map(availabilityToXml)}
    </external>

  def availabilityToXml(availability: Availability): Elem =
      <availability start={availability.start.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} end={availability.end.to.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)} preference={availability.preference.d.toString}/>

  def vivaToXml(viva: Viva): Elem =
    <viva student={viva.student.to} title={viva.title.to}>
      {viva.roles.map { case (id, role) =>
      role match
        case Role.President => <president id={id.to}/>
        case Role.Advisor => <advisor id={id.to}/>
        case Role.Supervisor => <supervisor id={id.to}/>
        case Role.Coadvisor => <coadvisor id={id.to}/>
    }}
    </viva>
