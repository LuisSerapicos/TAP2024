package pj.domain.schedule.ScheduleMS03Tests

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.schedule.Domain.{Availability2, Resource, Role2, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.Utils.{anyOverlap, dateFormatter, fixAvailabilities, getRoleFromViva, overlaps, parseDate, parseDuration, sequence, stringToDuration}
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate, resourceId, resourceName, vivaStudent, vivaTitle}

import java.time.{Duration, LocalDateTime}

class UtilsTests extends AnyFunSuite:
  //Utils tests
  test("stringToDuration should return correct Duration for valid input"):
    val duration = "01:30:00"
    val expectedDuration = Duration.ofHours(1).plusMinutes(30)
    assert(stringToDuration(duration) == Some(expectedDuration))

  test("stringToDuration should return None for invalid input"):
    val duration = "invalid"
    assert(stringToDuration(duration) == None)

  test("parseDuration should return Duration.ZERO for invalid input"):
    val duration = agendaDuration.from("invalid").fold(
      error => fail("Unexpected error: " + error),
      duration => duration
    )
    assert(parseDuration(duration) == Duration.ZERO)

  test("parseDuration should return correct Duration for valid input"):
    val duration = agendaDuration.from("01:30:00").fold(
      error => fail("Unexpected error: " + error),
      duration => duration
    )
    val expectedDuration = Duration.ofHours(1).plusMinutes(30)
    assert(parseDuration(duration) == expectedDuration)

  test("parseDate should return correct LocalDateTime for valid input"):
    val dateString = "2024-06-01T12:59:59"
    val expectedDateTime = LocalDateTime.of(2024, 6, 1, 12, 59, 59)
    assert(parseDate(dateString) == expectedDateTime)

  test("parseDate should throw an exception for invalid input"):
    val dateString = "2024-6-1T12:59:59"
    assertThrows[Exception]:
      parseDate(dateString)

  test("dateFormatter should return correct formatted string for valid input"):
    val dateTime = LocalDateTime.of(2024, 6, 1, 12, 59, 59)
    val expectedDateString = "2024-06-01T12:59:59"
    assert(dateFormatter(dateTime) == expectedDateString)

  test("getRoleFromViva should return correct role for existing resource"):
    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val availability = Availability2.create(startF, endF, preference).fold(
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
    val resource1 = Resource.create(resource1Id, resource1Name, List(availability), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )

    val viva = Viva(student, title, List(resource1))
    assert(getRoleFromViva(viva, resource1.id) == Role2.President)

  test("getRoleFromViva should return Role2.None for non-existing resource"):
    val start = LocalDateTime.of(2024, 6, 1, 11, 0, 0)
    val end = LocalDateTime.of(2024, 6, 1, 12, 0, 0)
    val startF = availabilityDate.from(start)
    val endF = availabilityDate.from(end)
    val preference = Preference(3)

    val availability = Availability2.create(startF, endF, preference).fold(
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
    val resource1 = Resource.create(resource1Id, resource1Name, List(availability), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      id => id
    )

    val resource2Name = resourceName.from("Teacher 002").fold(
      error => fail("Unexpected error: " + error),
      name => name
    )
    val resource2 = Resource.create(resource2Id, resource2Name, List(availability), Role2.President).fold(
      error => fail("Unexpected error: " + error),
      resource => resource
    )

    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )

    val viva = Viva(student, title, List(resource1))
    assert(getRoleFromViva(viva, resource2.id) == Role2.None)

  test("overlaps function should return true when there is an overlap"):
    val availability1 = Availability2(availabilityDate.from(LocalDateTime.now()), availabilityDate.from(LocalDateTime.now().plusHours(2)), Preference(1))
    val availability2 = availability1

    val student1 = vivaStudent.from("student1").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val student2 = vivaStudent.from("student2").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("title").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )
    val viva1 = Viva(student1, title, List.empty)
    val viva2 = Viva(student2, title, List.empty)

    val slot1 = (viva1, List((1, List(availability1))))
    val slot2 = (viva2, List((1, List(availability2))))
    assert(overlaps(slot1, slot2))

  test("overlaps function should return false when there is no overlap"):
    val availability1 = Availability2(availabilityDate.from(LocalDateTime.now()), availabilityDate.from(LocalDateTime.now().plusHours(2)), Preference(1))
    val availability2 = Availability2(availabilityDate.from(LocalDateTime.now().plusHours(3)), availabilityDate.from(LocalDateTime.now().plusHours(5)), Preference(1))

    val student1 = vivaStudent.from("student1").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val student2 = vivaStudent.from("student2").fold(
      error => fail("Unexpected error: " + error),
      student => student
    )
    val title = vivaTitle.from("title").fold(
      error => fail("Unexpected error: " + error),
      title => title
    )
    val viva1 = Viva(student1, title, List.empty)
    val viva2 = Viva(student2, title, List.empty)

    val slot1 = (viva1, List((1, List(availability1))))
    val slot2 = (viva2, List((1, List(availability2))))
    assert(!overlaps(slot1, slot2))

  test("Sequence function should transform a list of list of vivas and list of availabilities into a list of list of tuples of viva and availability"):
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

    val viva1 = Viva.create(student, title, roles).fold(
      error => fail("Unexpected error: " + error),
      viva => viva
    )
    
    val viva2 = Viva.create(student, title, roles).fold(
      error => fail("Unexpected error: " + error),
      viva => viva
    )
    
    val input = List(List(viva1, viva2), List(availability, availability2))
    val result = sequence(input)
    val expected = List(List(viva1, availability), List(viva1, availability2), List(viva2, availability), List(viva2, availability2))

    assert(result == expected)


  test("fixAvailabilities function should update the resources availabilities with two new availabilities because the overlap is in the middle"):
    val availability = Availability2(availabilityDate.from(LocalDateTime.now()),
      availabilityDate.from(LocalDateTime.now().plusHours(3)),
      Preference(1))

    val overlapStart = availabilityDate.from(LocalDateTime.now().plusHours(1))
    val overlapEnd = availabilityDate.from(LocalDateTime.now().plusHours(2))

    val result = fixAvailabilities(availability, overlapStart, overlapEnd)

    val expected = List(
      Availability2(availability.start, overlapStart, availability.preference),
      Availability2(overlapEnd, availability.end, availability.preference)
    )

    assert(result == expected)

  test("fixAvailabilities function should update the resources availabilities with a new availability because the overlap is in the start of the availability"):
    val availability = Availability2(availabilityDate.from(LocalDateTime.now()),
      availabilityDate.from(LocalDateTime.now().plusHours(3)),
      Preference(1))

    val overlapStart = availabilityDate.from(LocalDateTime.now())
    val overlapEnd = availabilityDate.from(LocalDateTime.now().plusHours(1))

    val result = fixAvailabilities(availability, overlapStart, overlapEnd)

    val expected = List(
      Availability2(overlapEnd, availability.end, availability.preference)
    )

    assert(result == expected)

  test("fixAvailabilities function should update the resources availabilities with a new availability because the overlap is in the end of the availability"):
    val availability = Availability2(availabilityDate.from(LocalDateTime.now()),
      availabilityDate.from(LocalDateTime.now().plusHours(3)),
      Preference(1))

    val overlapStart = availabilityDate.from(LocalDateTime.now().plusHours(2))
    val overlapEnd = availabilityDate.from(LocalDateTime.now().plusHours(3))

    val result = fixAvailabilities(availability, overlapStart, overlapEnd)

    val expected = List(
      Availability2(availability.start, overlapStart, availability.preference)
    )

    assert(result == expected)

  test("fixAvailabilities function should update the resources availabilities with an empty list because the overlap is the same as the availability"):
    val availability = Availability2(availabilityDate.from(LocalDateTime.now()),
      availabilityDate.from(LocalDateTime.now().plusHours(3)),
      Preference(1))

    val overlapStart = availability.start
    val overlapEnd = availability.end

    val result = fixAvailabilities(availability, overlapStart, overlapEnd)

    val expected = List.empty

    assert(result == expected)


