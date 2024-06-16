package pj.domain.schedule.ScheduleMS03Tests

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError
import pj.domain.schedule.Domain.{Availability2, Resource, Role2, Viva}
import pj.domain.schedule.{Algorithm, Preference}
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityDate, resourceId, resourceName, vivaStudent, vivaTitle}

import java.time.LocalDateTime
import scala.xml.Elem

class AlgorithmTests extends AnyFunSuite:

  test("intersectAvailabilities should return the correct overlapping availabilities"):
    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource1Name = resourceName.from("Teacher 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    
    val currentResource = Resource(
      id = resource1Id,
      name = resource1Name,
      role = Role2.President,
      availabilities = List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 9, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 12, 0)),
          preference = Preference(1)
        )
      )
    )

    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource2Name = resourceName.from("Teacher 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val resource3Id = resourceId.from("E001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource3Name = resourceName.from("External 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val otherResources = List(
      Resource(
        id = resource2Id,
        name = resource2Name,
        role = Role2.Advisor,
        availabilities = List(
          Availability2(
            start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 10, 0)),
            end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)),
            preference = Preference(1)
          )
        )
      ),
      Resource(
        id = resource3Id,
        name = resource3Name,
        role = Role2.Supervisor,
        availabilities = List(
          Availability2(
            start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)),
            end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 13, 0)),
            preference = Preference(1)
          )
        )
      )
    )

    val duration = agendaDuration.from("01:00:00").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val result = Algorithm.intersectAvailabilities(currentResource, otherResources, duration)

    assert(result.sizeIs == 1)
    result.headOption match
      case Some(availability) =>
        assert(availability.start == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 9, 0)))
        assert(availability.end == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 12, 0)))
      case None => fail("No availability found")

  test("isResourceAvailable should return the correct availabilities"):
    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource1Name = resourceName.from("Teacher 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource2Name = resourceName.from("Teacher 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource3Id = resourceId.from("E001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource3Name = resourceName.from("External 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val viva = Viva(
      student,
      title,
      roles = List(
        Resource(
          resource1Id,
          resource1Name,
          role = Role2.President,
          availabilities = List(
            Availability2(
              start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 9, 0)),
              end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 12, 0)),
              preference = Preference(3)
            )
          )
        ),
        Resource(
          resource2Id,
          resource2Name,
          role = Role2.Advisor,
          availabilities = List(
            Availability2(
              start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 10, 0)),
              end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)),
              preference = Preference(1)
            )
          )
        )
      )
    )

    val duration = agendaDuration.from("01:00:00").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val result = Algorithm.isResourceAvailable(viva, duration)

    result match
      case Right((viva, availabilities)) =>
        assert(availabilities.sizeIs == 1)
        availabilities.headOption match
          case Some(availability) =>
            assert(availability.start == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 10, 0)))
            assert(availability.end == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)))
            assert(availability.preference == Preference(4))
          case None => fail("No availability found")
      case Left(DomainError.ImpossibleSchedule) => fail("Expected Right but got Left with error: ImpossibleSchedule")
      case _ => fail("Unexpected error")

  test("findAllTimeSlots should return the correct time slots"):
    val suitableAvailabilities = List(
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 9, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 12, 0)),
        preference = Preference(2)
      ),
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 10, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)),
        preference = Preference(5)
      )
    )

    val duration = agendaDuration.from("01:00:00").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val numResources = 2

    val result = Algorithm.findAllTimeSlots(suitableAvailabilities, duration, numResources)

    assert(result.sizeIs == 1)
    result.headOption match
      case Some((preference, availabilities)) =>
        assert(preference == 7)
        assert(availabilities.sizeIs == 1)
        availabilities.headOption match
          case Some(availability) =>
            assert(availability.start == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 10, 0)))
            assert(availability.end == availabilityDate.from(LocalDateTime.of(2024, 6, 1, 11, 0)))
          case None => fail("No availability found")
      case None => fail("No time slots found")

  test("findLatestTimeSlotWithNoViva should return the correct time slot"):
    val suitableAvailabilities = List(
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 9, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 12, 0)),
        preference = Preference(2)
      ),
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 10, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 11, 30)),
        preference = Preference(5)
      ),
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 14, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 18, 0)),
        preference = Preference(3)
      ),
      Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 17, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 18, 30)),
        preference = Preference(4)
      )
    )

    val duration = agendaDuration.from("01:30:00").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val result = Algorithm.findLatestTimeSlotWithNoViva(suitableAvailabilities, duration)

    result match
      case Some((start, end, preference)) =>
        assert(start == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 17, 0)))
        assert(end == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 18, 30)))
        assert(preference == 14)
      case None => fail("No time slot found")

  test("scheduleAllVivas should return the correct resources and vivas"):
    val student = vivaStudent.from("Student 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val title = vivaTitle.from("Title 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val student2 = vivaStudent.from("Student 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val title2 = vivaTitle.from("Title 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val resource1Id = resourceId.from("T001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource1Name = resourceName.from("Teacher 001").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource2Id = resourceId.from("T002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource2Name = resourceName.from("Teacher 002").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource3Id = resourceId.from("T003").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource3Name = resourceName.from("Teacher 003").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource4Id = resourceId.from("T004").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    val resource4Name = resourceName.from("Teacher 004").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )
    
    val resources = List(
      Resource(resource1Id, resource1Name, List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 9, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 12, 0)),
          preference = Preference(2)
        )
      ), Role2.President),
      Resource(resource2Id, resource2Name, List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 10, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 12, 0)),
          preference = Preference(5)
        )
      ), Role2.Advisor)
    )
    val resources2 = List(
      Resource(resource3Id, resource3Name, List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 14, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 17, 0)),
          preference = Preference(3)
        )
      ), Role2.President),
      Resource(resource4Id, resource4Name, List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 14, 30)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 18, 30)),
          preference = Preference(1)
        )
      ), Role2.Advisor)
    )

    val vivas = List(
      Viva(student, title, resources),
      Viva(student2, title2, resources2)
    )

    val duration = agendaDuration.from("02:00:00").fold(
      error => fail("Unexpected error: " + error),
      value => value
    )

    val result = Algorithm.scheduleAllVivas(vivas, resources ++ resources2, duration)

    result match
      case Right((updatedResources, scheduledVivas)) =>
        assert(updatedResources.sizeIs == 4)
        assert(scheduledVivas.sizeIs == 2)

        val sortedScheduledVivas = scheduledVivas.sortBy(_._1.student.to) // Sort by student name for consistent order

        sortedScheduledVivas match
          case List((viva1, (start1, end1, preference1)), (viva2, (start2, end2, preference2))) =>
            assert(viva1.student == student)
            assert(start1 == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 10, 0)))
            assert(end1 == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 12, 0)))
            assert(preference1 == 7)

            assert(viva2.student == student2)
            assert(start2 == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 14, 30)))
            assert(end2 == availabilityDate.from(LocalDateTime.of(2024, 1, 6, 16, 30)))
            assert(preference2 == 4)

          case _ => fail("Unexpected number of scheduled vivas")

      case Left(error) => fail("Unexpected error: " + error)

  test("findGlobalPreference should return the correct total preference and scheduled vivas"):
    val resources = List(
      Resource(resourceId.from("T001").getOrElse(fail("Unexpected error")), resourceName.from("Teacher 001").getOrElse(fail("Unexpected error")), List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 9, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 12, 0)),
          preference = Preference(2)
        )
      ), Role2.President),
      Resource(resourceId.from("T002").getOrElse(fail("Unexpected error")), resourceName.from("Teacher 002").getOrElse(fail("Unexpected error")), List(
        Availability2(
          start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 10, 0)),
          end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 11, 0)),
          preference = Preference(5)
        )
      ), Role2.Advisor)
    )

    val vivas = List(
      Viva(vivaStudent.from("Student 001").getOrElse(fail("Unexpected error")), vivaTitle.from("Title 001").getOrElse(fail("Unexpected error")), resources),
      Viva(vivaStudent.from("Student 002").getOrElse(fail("Unexpected error")), vivaTitle.from("Title 002").getOrElse(fail("Unexpected error")), resources)
    )

    val duration = agendaDuration.from("01:00:00").getOrElse(fail("Unexpected error"))

    val allTimeSlots = List(
      List((14, List(Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 10, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 11, 0)),
        preference = Preference(7)
      )))),
      List((16, List(Availability2(
        start = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 14, 0)),
        end = availabilityDate.from(LocalDateTime.of(2024, 1, 6, 15, 0)),
        preference = Preference(7)
      ))))
    )

    val result = Algorithm.findGlobalPreference(Right(vivas), allTimeSlots)

    assert(result._1 == 30)
    assert(result._2.sizeIs == 2)

  test("scheduleVivasXML should return the correct XML output"):
    val xmlInput: Elem =
      <agenda duration="01:15:00">
        <vivas>
          <viva student="Student 001" title="Title 001">
            <president id="T001"/>
            <advisor id="T002"/>
          </viva>
        </vivas>
        <resources>
          <teachers>
            <teacher id="T001" name="Teacher 001">
              <availability start="2007-12-03T10:15:00" end="2007-12-03T11:30:00" preference="3"/>
            </teacher>
          </teachers>
          <externals>
            <external id="T002" name="Teacher 002">
              <availability start="2007-12-03T09:15:00" end="2007-12-03T11:30:00" preference="4"/>
            </external>
          </externals>
        </resources>
      </agenda>

    val expectedOutput: Elem =
      <schedule xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../schedule.xsd" totalPreference="7">
            <viva student="Student 001" title="Title 001" start="2007-12-03T10:15:00" end="2007-12-03T11:30:00" preference="7">
              <president name="Teacher 001"/><advisor name="Teacher 002"/>
            </viva>
          </schedule>


    val result = Algorithm.scheduleVivasXML(xmlInput)

    result match
      case Right(output) => assert(output == expectedOutput)
      case Left(error) => fail("Unexpected error: " + error)

