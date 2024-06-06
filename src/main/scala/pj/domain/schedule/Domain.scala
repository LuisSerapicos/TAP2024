package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.SimpleTypes.*
import pj.domain.schedule.Domain.{Availability2, Resource, Role2, Viva}


object Domain:

  case class Viva(student: vivaStudent, title: vivaTitle, roles: List[Resource]):
    // Ensure that all necessary roles are present
    def isValid: Boolean = roles.exists(_.role == Role2.President) && roles.exists(_.role == Role2.Advisor)


  case class Availability2(start: availabilityDate, end: availabilityDate, preference: Preference):
    // Ensure that start is before end
    def isValid: Boolean = start.isBefore(end)


  enum Role2:
    case President, Advisor, Coadvisor, Supervisor, None

  implicit val roleOrdering: Ordering[Role2] = Ordering.by:
    case Role2.President => 1
    case Role2.Advisor => 2
    case Role2.Coadvisor => 3
    case Role2.Supervisor => 4
    case _ => 5

  case class Resource(id: resourceId, name: resourceName, availabilities: List[Availability2], role: Role2):
    // Ensure all availabilities are valid
    def isValid: Boolean = availabilities.forall(_.isValid)



  object Viva:
    // Helper method to create and validate a Viva
    def create(student: vivaStudent, title: vivaTitle, roles: List[Resource]): Result[Viva] =
      val viva = Viva(student, title, roles)
      if viva.isValid then Right(viva) else Left(DomainError.InvalidNumberOfRoles("Viva must have at least one President and one Advisor"))


  object Availability2:
    // Helper method to create and validate an Availability
    def create(start: availabilityDate, end: availabilityDate, preference: Preference): Result[Availability2] =
      val availability = Availability2(start, end, preference)
      if availability.isValid then Right(availability) else Left(DomainError.InvalidAvailabilityEnd("Availability end must be after start"))


  object Resource:
    // Helper method to create and validate a Resource
    def create(id: resourceId, name: resourceName, availabilities: List[Availability2], role: Role2): Result[Resource] =
      val resource = Resource(id, name, availabilities, role)
      if resource.isValid then Right(resource) else Left(DomainError.InvalidResourceId("Resource has invalid availabilities"))

