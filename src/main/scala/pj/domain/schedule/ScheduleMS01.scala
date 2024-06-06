package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.SimpleTypes.availabilityDate
import pj.domain.schedule.Utils.dateFormatter
import pj.domain.schedule.Domain.{Agenda, Role2, Viva, VivaDuration}
import scala.xml.Elem


object ScheduleMS01 extends Schedule:

  // TODO: Create the code to implement a functional domain model for schedule creation
  //       Use the xml.XML code to handle the xml elements
  //       Refer to https://github.com/scala/scala-xml/wiki/XML-Processing for xml creation
  def create(xml: Elem): Result[Elem] = Algorithm.scheduleVivasXML(xml)

