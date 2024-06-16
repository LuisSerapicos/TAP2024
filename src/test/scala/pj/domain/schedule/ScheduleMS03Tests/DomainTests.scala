package pj.domain.schedule.ScheduleMS03Tests

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.schedule.Domain.{Agenda, Availability2, Resource, Role2, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate, resourceId, resourceName, vivaStudent, vivaTitle}
import pj.domain.schedule.Utils.parseDuration

import java.time.{LocalDateTime, LocalTime}

class DomainTests extends AnyFunSuite:
  //Domain Tests
  test("Class VivaDuration should create a valid time"):
    val duration = agendaDuration.from("01:30:00").fold(
      error => fail("Unexpected error: " + error),
      duration => parseDuration(duration)
    )
    assert(duration.toSeconds == LocalTime.of(1, 30, 0).toSecondOfDay)

  test("Class Availability should be valid (start before end)"):
    val start = LocalDateTime.of(2024, 6, 1, 11, 59, 59)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)
    val availability = Availability2.create(startF, endF, preference)
    assert(availability.isRight)

  test("Class Availability returns left because start is after end"):
    val start = LocalDateTime.of(2024, 6, 1, 12, 1, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)
    val availability = Availability2.create(startF, endF, preference)
    assert(availability.isLeft)

  test("Class Resource should be valid"):
    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val duration = agendaDuration.from("04:00:00").fold(
      error => fail("Unexpected error: " + error),
      duration => parseDuration(duration)
    )

    val availability = Availability2.create(startF, endF, preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val availability2 = Availability2.create(startF.plusTime(duration), endF.plusTime(duration), preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )
    val resource1Name = resourceName.from("Teacher 001").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val role = Role2.President

    val resource1 = Resource.create(resource1Id, resource1Name, List(availability, availability2), role)
    assert(resource1.isRight)

  test("Class Viva should be valid (have mandatory roles - President and Advisor)"):
    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )

    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val duration = agendaDuration.from("04:00:00").fold(
      error => fail("Unexpected error: " + error),
      duration => parseDuration(duration)
    )

    val availability = Availability2.create(startF, endF, preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val availability2 = Availability2.create(startF.plusTime(duration), endF.plusTime(duration), preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource1Name = resourceName.from("T001").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource2Name = resourceName.from("T002").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource1 = Resource.create(resource1Id, resource1Name, List(availability, availability2), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )
    val resource2 = Resource.create(resource2Id, resource2Name, List(availability, availability2), Role2.Advisor).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val roles = List(resource1, resource2)

    val newViva = Viva.create(student, title, roles)
    assert(newViva.isRight)

  test("Class Viva returns left because it does not have mandatory roles - President and Advisor"):
    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )

    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val duration = agendaDuration.from("04:00:00").fold(
      error => fail("Unexpected error: " + error),
      duration => parseDuration(duration)
    )

    val availability = Availability2.create(startF, endF, preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val availability2 = Availability2.create(startF.plusTime(duration), endF.plusTime(duration), preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource1Name = resourceName.from("T001").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource2Name = resourceName.from("T002").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource1 = Resource.create(resource1Id, resource1Name, List(availability, availability2), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )
    val resource2 = Resource.create(resource2Id, resource2Name, List(availability, availability2), Role2.Coadvisor).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val roles = List(resource1, resource2)

    val viva = Viva.create(student, title, roles)
    assert(viva.isLeft)

  test("Class Agenda should be valid"):
    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )

    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val duration = agendaDuration.from("04:00:00").fold(
      error => fail("Unexpected error: " + error),
      duration => parseDuration(duration)
    )

    val availability = Availability2.create(startF, endF, preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val availability2 = Availability2.create(startF.plusTime(duration), endF.plusTime(duration), preference).fold(
      error => fail("Unexpected error: " + error),
      availability => availability
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource1Name = resourceName.from("T001").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource2Name = resourceName.from("T002").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )

    val resource1 = Resource.create(resource1Id, resource1Name, List(availability, availability2), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )
    val resource2 = Resource.create(resource2Id, resource2Name, List(availability, availability2), Role2.Advisor).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val roles = List(resource1, resource2)

    val viva = Viva.create(student, title, roles).fold(
      error => fail("Unexpected error: " + error),
      viva => viva
    )

    val totalPreference = availability.preference.d + availability2.preference.d

    val agenda = Agenda.create(viva, startF, endF, totalPreference)
    assert(agenda.totalVivaPreference > 0)

