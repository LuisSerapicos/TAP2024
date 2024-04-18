package pj.domain.schedule

import pj.domain.schedule.SimpleTypes.*

import java.time.LocalDateTime

object Domain:

  case class Viva(student: vivaStudent, title: vivaTitle, roles: Map[resourceId, Role])
  
  case class Availability(start: availabilityStart, end: availabilityEnd, preference: Preference)

  case class Resource(id: resourceId, name: resourceName, availabilities: List[Availability], roles: Map[vivaId, Role])
