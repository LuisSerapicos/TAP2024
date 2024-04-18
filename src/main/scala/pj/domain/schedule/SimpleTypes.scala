package pj.domain.schedule


import java.time.LocalDateTime
import pj.domain.{DomainError, Result}
import pj.domain.DomainError.*

object SimpleTypes:

  opaque type agendaDuration = String

  object agendaDuration:
    def from(duration: String): Result[agendaDuration] =
      if (!duration.isEmpty) Right(duration)
      else Left(DomainError.InvalidAgendaDuration(duration))

    extension (d: agendaDuration)
      def to: String = d


  opaque type vivaStudent = String

  object vivaStudent:
    def from(student: String): Result[vivaStudent] =
      if (student.nonEmpty) Right(student)
      else Left(DomainError.InvalidVivaStudent(student))

    extension (s: vivaStudent)
      def to: String = s


  opaque type vivaTitle = String

  object vivaTitle:
    def from(title: String): Result[vivaTitle] =
      if (title.nonEmpty) Right(title)
      else Left(DomainError.InvalidVivaTitle(title))

    extension (t: vivaTitle)
      def to: String = t


  opaque type teacherId = String

  object teacherId:
    def from(id: String): Result[teacherId] =
      if (id.matches("^T\\d{3}$")) Right(id)
      else Left(DomainError.InvalidTeacherId(id))

    extension (id: teacherId)
      def to: String = id


  opaque type teacherName = String

  object teacherName:
    def from(name: String): Result[teacherName] =
      if (name.nonEmpty) Right(name)
      else Left(DomainError.InvalidTeacherName(name))

    extension (name: teacherName)
      def to: String = name


  opaque type availabilityStart = LocalDateTime

  object availabilityStart:
    def from(start: LocalDateTime): Result[availabilityStart] =
      if start.toString == "" then Left(DomainError.InvalidAvailabilityStart(start.toString))
      else Right(start)

    extension (start: availabilityStart)
      def to: LocalDateTime = start


  opaque type availabilityEnd = LocalDateTime

  object availabilityEnd:
    def from(end: LocalDateTime): Result[availabilityEnd] =
      if end.toString == "" then Left(DomainError.InvalidAvailabilityEnd(end.toString))
      else Right(end)

    extension (end: availabilityEnd)
      def to: LocalDateTime = end


  opaque type externalId = String

  object externalId:
    def from(id: String): Result[externalId] =
      if (id.matches("^E\\d{3}$")) Right(id)
      else Left(DomainError.InvalidExternalId(id))

    extension (id: externalId)
      def to: String = id


  opaque type externalName = String

  object externalName:
    def from(name: String): Result[externalName] =
      if (name.nonEmpty) Right(name)
      else Left(DomainError.InvalidExternalName(name))

    extension (name: externalName)
      def to: String = name


  opaque type vivaId = String

  object vivaId:
    def from(id: String): Result[vivaId] =
      if (id.nonEmpty) Right(id)
      else Left(DomainError.InvalidVivaId(id))

    extension (id: vivaId)
      def to: String = id

  opaque type resourceId = String

  object resourceId:
    def from(id: String): Result[resourceId] =
      if (id.matches("^T\\d{3}$") || id.matches("^E\\d{3}$")) Right(id)
      else Left(DomainError.InvalidResourceId(id))

    extension (id: resourceId)
      def to: String = id

  opaque type resourceName = String

  object resourceName:
    def from(name: String): Result[resourceName] =
      if (name.nonEmpty) Right(name)
      else Left(DomainError.InvalidResourceName(name))

    extension (name: resourceName)
      def to: String = name

  enum Role:
    case President, Advisor, Coadvisor, Supervisor

  implicit val roleOrdering: Ordering[Role] = Ordering.by:
    case Role.President => 1
    case Role.Advisor => 2
    case Role.Supervisor => 3
    case Role.Coadvisor => 4


final case class Preference private(d: Int)

object Preference:

  private def unsafePreference(p: Int): Preference = Preference(p)

  private val isValid: Int => Boolean = { i => i >= 1 && i <= 5 }

  def from(preference: String): Result[Preference] =
    try
      val prefInt = preference.toInt
      if (isValid(prefInt)) Right(unsafePreference(prefInt))
      else Left(DomainError.InvalidPreference(s"Invalid preference: $preference"))
    catch
      case _: NumberFormatException => Left(DomainError.InvalidPreference(s"Invalid preference: $preference"))

  extension (pref: Preference)
    def to: Int = pref.d
