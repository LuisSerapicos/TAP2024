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
