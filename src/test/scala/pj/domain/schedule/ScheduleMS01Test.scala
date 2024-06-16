package pj.domain.schedule

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, externalId, resourceId, resourceName, teacherId, vivaStudent, vivaTitle}
import pj.io.FileIO
import pj.xml.XML

import java.time.LocalDateTime
import scala.xml.Elem

// TODO: Create the code to test a functional domain model for schedule creation.
//       create files in the files/test/ms01 folder
class ScheduleMS01Test extends AnyFunSuite:
  //Domain Error Tests
  test("toString should return correct string for IOFileProblem"):
    val error = DomainError.IOFileProblem("file error")
    assert(error.toString == "IOFileProblem(file error)")

  test("toString should return correct string for XMLError"):
    val error = DomainError.XMLError("xml error")
    assert(error.toString == "XMLError(xml error)")

  test("toString should return correct string for InvalidAgendaDuration"):
    val error = DomainError.InvalidAgendaDuration("invalid duration")
    assert(error.toString == "InvalidAgendaDuration(invalid duration)")

  test("toString should return correct string for InvalidVivaStudent"):
    val error = DomainError.InvalidVivaStudent("invalid viva student")
    assert(error.toString == "InvalidVivaStudent(invalid viva student)")

  test("toString should return correct string for InvalidVivaTitle"):
    val error = DomainError.InvalidVivaTitle("invalid viva title")
    assert(error.toString == "InvalidVivaTitle(invalid viva title)")

  test("toString should return correct string for InvalidTeacherId"):
    val error = DomainError.InvalidTeacherId("invalid teacher id")
    assert(error.toString == "InvalidTeacherId(invalid teacher id)")

  test("toString should return correct string for InvalidTeacherName"):
    val error = DomainError.InvalidTeacherName("invalid teacher name")
    assert(error.toString == "InvalidTeacherName(invalid teacher name)")

  test("toString should return correct string for InvalidAvailabilityStart"):
    val error = DomainError.InvalidAvailabilityStart("invalid availability start")
    assert(error.toString == "InvalidAvailabilityStart(invalid availability start)")

  test("toString should return correct string for InvalidAvailabilityEnd"):
    val error = DomainError.InvalidAvailabilityEnd("invalid availability end")
    assert(error.toString == "InvalidAvailabilityEnd(invalid availability end)")

  test("toString should return correct string for InvalidExternalId"):
    val error = DomainError.InvalidExternalId("invalid external id")
    assert(error.toString == "InvalidExternalId(invalid external id)")

  test("toString should return correct string for InvalidExternalName"):
    val error = DomainError.InvalidExternalName("invalid external name")
    assert(error.toString == "InvalidExternalName(invalid external name)")

  test("toString should return correct string for InvalidPreference"):
    val error = DomainError.InvalidPreference("invalid preference")
    assert(error.toString == "InvalidPreference(invalid preference)")

  test("toString should return correct string for InvalidResourceId"):
    val error = DomainError.InvalidResourceId("invalid resource id")
    assert(error.toString == "InvalidResourceId(invalid resource id)")

  test("toString should return correct string for InvalidResourceName"):
    val error = DomainError.InvalidResourceName("invalid resource name")
    assert(error.toString == "InvalidResourceName(invalid resource name)")



  //XML Tests
  test("fromNode returns Right if node president exists"):
    val xml: Elem = <viva>
      <president/>
    </viva>
    val result = XML.fromNode(xml, "president")
    assert(result.isRight)

  test("fromNode returns Left if node president does not exist"):
    val xml: Elem = <viva>
      <advisor/>
    </viva>
    val result = XML.fromNode(xml, "president")
    assert(result.isLeft)

  test("fromAttribute returns Right if attribute student exists"):
    val xml: Elem = <viva student="Student 001"></viva>
    val result = XML.fromAttribute(xml, "student")
    assert(result.isRight)

  test("fromAttribute returns Left if attribute student does not exist"):
    val xml: Elem = <viva date="Date 001"></viva>
    val result = XML.fromAttribute(xml, "student")
    assert(result.isLeft)

  test("traverse returns Right if all viva elements are valid"):
    val list = Seq(
      <viva student="Student 001" title="Title 1">
        <president id="T001"/>
        <advisor id="T002"/>
        <supervisor id="E002"/>
      </viva>,
      <viva student="Student 002" title="Title 2">
        <president id="T001"/>
        <advisor id="T002"/>
        <supervisor id="E001"/>
      </viva>
    )
    val result = XML.traverse(list, viva => for {
      student <- XML.fromAttribute(viva, "student")
      title <- XML.fromAttribute(viva, "title")
      president <- XML.fromNode(viva, "president")
      advisor <- XML.fromNode(viva, "advisor")
    } yield (student, title, president, advisor))
    assert(result.isRight)

  test("traverse returns Left if a viva element is invalid"):
    val list = Seq(
      <viva student="Student 001" title="Title 1">
        <president id="T001"/>
        <advisor id="T002"/>
        <supervisor id="E002"/>
      </viva>,
      <viva student="Student 002" title="Title 2">
        <president id="T001"/>
        <supervisor id="E001"/>
      </viva>
    )
    val result = XML.traverse(list, viva => for {
      student <- XML.fromAttribute(viva, "student")
      title <- XML.fromAttribute(viva, "title")
      president <- XML.fromNode(viva, "president")
      advisor <- XML.fromNode(viva, "advisor")
    } yield (student, title, president, advisor))
    assert(result.isLeft)



  //FileIO Tests
  test("load with filename returns Right with XML Elem when file exists"):
    val result = FileIO.load("files/assessment/ms01/valid_agenda_01_in.xml")
    assert(result.isRight)

  test("load with filename returns Left with IOFileProblem when file does not exist"):
    val result = FileIO.load("files/assessment/ms01/invalid_file.xml")
    assert(result.isLeft)

  test("loadError returns Right with message when file exists and has message attribute"):
    val result = FileIO.loadError("files/assessment/ms01/invalid_agenda_01_outError.xml")
    assert(result.isRight)

  test("loadError returns Left with IOFileProblem when file does not exist"):
    val result = FileIO.loadError("files/assessment/ms01/non_existing_file.xml")
    assert(result.isLeft)

  test("loadError returns Left with IOFileProblem when file does not have message attribute"):
    val result = FileIO.loadError("files/assessment/ms01/valid_agenda_01_in.xml")
    assert(result.isLeft)


  //SimpleTypes Tests
  test("agendaDuration from should return Right when valid duration"):
    val result = agendaDuration.from("22:30:00")
    assert(result.isRight)

  test("vivaStudent from should return Right when valid student"):
    val result = vivaStudent.from("Student 001")
    assert(result.isRight)

  test("vivaStudent from should return Left when invalid student"):
    val result = vivaStudent.from("")
    assert(result.isLeft)

  test("vivaTitle from should return Right when valid title"):
    val result = vivaTitle.from("Title 1")
    assert(result.isRight)

  test("vivaTitle from should return Left when invalid title"):
    val result = vivaTitle.from("")
    assert(result.isLeft)

  test("teacherId from should return Right when valid id"):
    val result = teacherId.from("T001")
    assert(result.isRight)

  test("teacherId from should return Left when invalid id"):
    val result = teacherId.from("T01")
    assert(result.isLeft)

  test("availabilityStart from should return Right when valid start"):
    val result1 = availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
    assert(result1.isRight)

  test("availabilityEnd from should return Right when valid end"):
    val result = availabilityEnd.from(LocalDateTime.parse("2007-12-03T10:15:00"))
    assert(result.isRight)

  test("externalId from should return Right when valid id"):
    val result = externalId.from("E001")
    assert(result.isRight)

  test("externalId from should return Left when invalid id"):
    val result = externalId.from("E01")
    assert(result.isLeft)

  test("resourceId from should return Right when valid id"):
    val result = resourceId.from("T001")
    assert(result.isRight)

  test("resourceId from should return Left when invalid id"):
    val result = resourceId.from("T01")
    assert(result.isLeft)

  test("Preference from should return Right when valid preference"):
    val result = Preference.from("3")
    assert(result.isRight)

  test("Preference from should return Left when invalid preference"):
    val result = Preference.from("6")
    assert(result.isLeft)


  //XMLtoDomain Tests
  test("getAgendaDuration returns Right when valid XML"):
    val xml: Elem = <agenda duration="01:00:00"/>
    val result = XMLtoDomain.getAgendaDuration(xml)
    assert(result.isRight)

  test("getAgendaDuration returns Left when invalid XML"):
    val xml: Elem = <agenda/>
    val result = XMLtoDomain.getAgendaDuration(xml)
    assert(result.isLeft)

  test("getPreference returns Right when valid XML"):
    val xml: Elem = <agenda>
      <resources>
        <externals>
          <external id="E001" name="External 001">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="5"/>
          </external>
        </externals>
      </resources>
    </agenda>
    val result = XMLtoDomain.getPreference(xml)
    assert(result.isRight)

  test("getPreference returns Left when invalid XML"):
    val xml: Elem = <agenda>
      <resources>
        <teachers>
          <teacher id="E001" name="External 001">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00"/>
          </teacher>
        </teachers>
      </resources>
    </agenda>
    val result = XMLtoDomain.getPreference(xml)
    assert(result.isLeft)

  test("getAvailabilities should return list of Availability when valid XML"):
    val xml = <teacher>
      <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="5"/>
      <availability start="2007-12-04T10:15:00" end="2007-12-04T11:15:00" preference="3"/>
    </teacher>
    val result = XMLtoDomain.getAvailabilities(xml)
    assert(result.isRight)
    assert(result.getOrElse(List()).lengthIs == 2)

  test("getAvailabilities should return Left when invalid XML"):
    val xml = <teacher>
      <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="5"/>
      <availability start="2007-12-04T10:15:00" preference="3"/>
    </teacher>
    val result = XMLtoDomain.getAvailabilities(xml)
    assert(result.isLeft)

  test("getRoles should return map of roles when valid XML"):
    val xml = <viva student="Student 001" title="Title 1">
      <president id="T001"/>
      <advisor id="T002"/>
    </viva>
    val result = XMLtoDomain.getRoles(xml)
    assert(result.isRight)

  test("getRoles should return no sufficient roles, if there isn't at least 2 roles"):
    val xml = <viva student="Student 001" title="Title 1">
      <president id="T001"/>
    </viva>
    val result = XMLtoDomain.getRoles(xml)
    assert(result.isLeft)

  test("getResource should return Right if XML provided is correct"):
    val xml = <teacher id="T001" name="John Doe">
      <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
    </teacher>
    val result = XMLtoDomain.getResource(xml)
    assert(result.isRight)

  test("getResource should return Left if resource tag is invalid (not teacher or external)"):
    val xml = <auxiliar id="T001" name="John Doe">
      <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
    </auxiliar>
    val result = XMLtoDomain.getResource(xml)
    assert(result.isLeft)

  test("getResource should Left if any attribute is missing"):
    val xml = <teacher name="Teacher 001">
      <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
    </teacher>
    val result = XMLtoDomain.getResource(xml)
    assert(result.isLeft)

  test("getViva should return Right if XML provided is correct"):
    val xml = <viva student="Student 001" title="Title 1">
      <president id="T001"/>
      <advisor id="T002"/>
    </viva>
    val result = XMLtoDomain.getViva(xml)
    assert(result.isRight)

  test("getViva should Left if any attribute is missing"):
    val xml = <viva student="Student 001">
      <president id="T001"/>
      <advisor id="T002"/>
    </viva>
    val result = XMLtoDomain.getViva(xml)
    assert(result.isLeft)

  test("getVivas returns Right when valid XML"):
    val xml: Elem = <agenda>
      <vivas>
        <viva student="John Doe" title="Thesis Title">
          <president id="T001"/>
          <advisor id="T002"/>
        </viva>
      </vivas>
    </agenda>
    val result = XMLtoDomain.getVivas(xml)
    assert(result.isRight)

  test("getVivas returns Left when invalid XML"):
    val xml: Elem = <agenda>
      <viva student="John Doe" title="Thesis Title">
        <president id="T001"/>
        <advisor id="T002"/>
      </viva>
    </agenda>
    val result = XMLtoDomain.getVivas(xml)
    assert(result.isLeft)

  test("getTeachers returns Right if the teachers resource is complete"):
    val xml: Elem = <agenda>
      <resources>
        <teachers>
          <teacher id="T001" name="Teacher 001">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
          </teacher>
        </teachers>
      </resources>
    </agenda>
    val result = XMLtoDomain.getTeachers(xml)
    assert(result.isRight)

  test("getTeachers returns Left if the teachers resource is not complete"):
    val xml: Elem = <agenda>
      <resources>
        <teachers>
          <teacher id="T001" name="Teacher 001">
          </teacher>
        </teachers>
      </resources>
    </agenda>
    val result = XMLtoDomain.getTeachers(xml)
    assert(result.isLeft)

  test("getExternals returns Right if the externals resource is complete"):
    val xml: Elem = <agenda>
      <resources>
        <externals>
          <external id="E001" name="External 001">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
          </external>
        </externals>
      </resources>
    </agenda>
    val result = XMLtoDomain.getExternals(xml)
    assert(result.isRight)

  test("getExternals returns Left if the externals resource is not complete"):
    val xml: Elem = <agenda>
      <resources>
        <externals>
          <external id="E001" name="External 001"></external>
        </externals>
      </resources>
    </agenda>
    val result = XMLtoDomain.getExternals(xml)
    assert(result.isLeft)

  //Algorithm Tests
  test("intersectAvailabilities correctly identifies overlapping availabilities and returns new availability"):
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T12:15:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val availability2Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T09:00:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T13:30:00"))
      preference <- Preference.from("5")
    } yield Availability(start, end, preference)

    val availability3Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T09:00:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T13:00:00"))
      preference <- Preference.from("3")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
      availability3 <- availability3Result
    } yield Resource(id, name, List(availability1, availability3))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability2 <- availability2Result
    } yield Resource(id, name, List(availability2))

    val vivaResult = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId <- resourceId.from("T001")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId -> Role.President))

    val agendaDurationResult = agendaDuration.from("01:00:00")
    agendaDurationResult match
      case Right(duration) =>
        (teacherResult, externalResult, vivaResult) match
          case (Right(teacher), Right(external), Right(viva)) =>
            val result = Algorithm.intersectAvailabilities(viva, List(teacher), List(external), duration)
            result match
              case Right(availabilities) =>
                assert(availabilities.lengthIs == 2)
                availabilities.headOption match
                  case Some(availability) =>
                    availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00")) match
                      case Right(expectedStart) =>
                        assert(availability.start == expectedStart)
                      case Left(_) => DomainError.InvalidAvailabilityStart("Invalid start time")
                    availabilityEnd.from(LocalDateTime.parse("2007-12-03T12:15:00")) match
                      case Right(expectedEnd) =>
                        assert(availability.end == expectedEnd)
                      case Left(_) => DomainError.InvalidAvailabilityEnd("Invalid end time")
                  case None => DomainError.XMLError("No availability found")
              case Left(_) => DomainError.XMLError("Invalid availability")
          case _ => DomainError.XMLError("Invalid teacher, external or viva")
      case Left(_) => DomainError.InvalidAgendaDuration("Invalid duration")

  test("intersectAvailabilities returns Left if there is no availability overlap"):
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T10:30:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val availability2Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-02T09:00:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-02T10:00:00"))
      preference <- Preference.from("5")
    } yield Availability(start, end, preference)

    val availability3Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-04T13:00:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-04T14:00:00"))
      preference <- Preference.from("3")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
      availability3 <- availability3Result
    } yield Resource(id, name, List(availability1, availability3))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability2 <- availability2Result
    } yield Resource(id, name, List(availability2))

    val vivaResult = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val agendaDurationResult = agendaDuration.from("01:00:00")
    agendaDurationResult match
      case Right(duration) =>
        (teacherResult, externalResult, vivaResult) match
          case (Right(teacher), Right(external), Right(viva)) =>
            val result = Algorithm.intersectAvailabilities(viva, List(teacher), List(external), duration)
            result match
              case Right(availabilities) =>
                assert(availabilities.isEmpty)
              case Left(_) => DomainError.XMLError("Invalid availability")
          case _ => DomainError.XMLError("Invalid teacher, external or viva")
      case Left(_) => DomainError.InvalidAgendaDuration("Invalid duration")


  test("findEarliestTimeSlot should return the earliest time slot that fits the agenda duration"):
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T11:15:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val vivaResult = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val durationResult = agendaDuration.from("01:00:00")

    (teacherResult, externalResult, vivaResult, durationResult) match
      case (Right(teacher), Right(external), Right(viva), Right(duration)) =>
        val result = Algorithm.findEarliestTimeSlot(viva, List(teacher), List(external), duration)
        result match
          case Right(Some(availability)) =>
            availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00")) match
              case Right(expectedStart) =>
                assert(availability.start == expectedStart)
              case Left(_) => DomainError.InvalidAvailabilityStart("Invalid start time")
            availabilityEnd.from(LocalDateTime.parse("2007-12-03T11:15:00")) match
              case Right(expectedEnd) =>
                assert(availability.end == expectedEnd)
              case Left(_) => DomainError.InvalidAvailabilityEnd("Invalid end time")
          case Right(None) => DomainError.XMLError("No availability found")
          case Left(_) => DomainError.XMLError("Invalid availability")

  test("scheduleViva should return Right with updated teachers and externals when valid inputs are provided"):
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T11:15:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val vivaResult = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val durationResult = agendaDuration.from("01:00:00")

    (teacherResult, externalResult, vivaResult, durationResult) match
      case (Right(teacher), Right(external), Right(viva), Right(duration)) =>
        val result = Algorithm.scheduleViva(viva, List(teacher), List(external), duration)
        result match
          case Right((scheduledViva, timeSlot, updatedTeachers, updatedExternals, vivaPreference)) =>
            assert(scheduledViva == viva)
            availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00")) match
              case Right(expectedStart) =>
                assert(timeSlot.start == expectedStart)
              case Left(_) => DomainError.InvalidAvailabilityStart("Invalid start time")
            availabilityEnd.from(LocalDateTime.parse("2007-12-03T11:15:00")) match
              case Right(expectedEnd) =>
                assert(timeSlot.end == expectedEnd)
              case Left(_) => DomainError.InvalidAvailabilityEnd("Invalid end time")
            assert(updatedTeachers.lengthIs == 1)
            assert(updatedExternals.lengthIs == 1)
            assert(vivaPreference == 4)
          case Left(_) => DomainError.XMLError("Invalid availability")
      case _ => DomainError.XMLError("Invalid teacher, external or viva")



  test("scheduleViva should return Left with DomainError.ImpossibleSchedule when no time slot can be found"):
    // Initialize viva, teachers, externals, and agendaDuration here with no possible time slot
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T11:15:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val vivaResult = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val durationResult = agendaDuration.from("01:00:00")

    (teacherResult, externalResult, vivaResult, durationResult) match
      case (Right(teacher), Right(external), Right(viva), Right(duration)) =>
        val result = Algorithm.scheduleViva(viva, List(teacher), List(external), duration)
        assert(result == Left(DomainError.ImpossibleSchedule))
      case _ => DomainError.XMLError("Invalid teacher, external or viva")


  test("scheduleAllVivas should return Right with scheduled vivas, updated teachers, updated externals, and total preference when valid inputs are provided"):
    val availability1Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T09:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T12:15:00"))
      preference <- Preference.from("4")
    } yield Availability(start, end, preference)

    val availability2Result = for {
      start <- availabilityStart.from(LocalDateTime.parse("2007-12-03T09:15:00"))
      end <- availabilityEnd.from(LocalDateTime.parse("2007-12-03T13:15:00"))
      preference <- Preference.from("3")
    } yield Availability(start, end, preference)

    val teacherResult = for {
      id <- resourceId.from("T001")
      name <- resourceName.from("Teacher 001")
      availability1 <- availability1Result
      availability2 <- availability2Result
    } yield Resource(id, name, List(availability1, availability2))

    val externalResult = for {
      id <- resourceId.from("E002")
      name <- resourceName.from("External 002")
      availability1 <- availability1Result
    } yield Resource(id, name, List(availability1))

    val viva1Result = for {
      student <- vivaStudent.from("Student 003")
      title <- vivaTitle.from("Title 3")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val viva2Result = for {
      student <- vivaStudent.from("Student 004")
      title <- vivaTitle.from("Title 4")
      resourceId1 <- resourceId.from("T001")
      resourceId2 <- resourceId.from("E002")
      teacher <- teacherResult
      external <- externalResult
    } yield Viva(student, title, Map(resourceId1 -> Role.President, resourceId2 -> Role.Advisor))

    val vivas = List(viva1Result, viva2Result)

    val agendaDurationResult = agendaDuration.from("01:00:00")

    (teacherResult, externalResult, vivas, agendaDurationResult) match
      case (Right(teacher), Right(external), List(Right(viva1), Right(viva2)), Right(duration)) =>
        val result = Algorithm.scheduleAllVivas(List(viva1, viva2), List(teacher), List(external), duration)
        println("Result" + result)
        result match
          case Right((scheduledVivas, updatedTeachers, updatedExternals, totalPreference)) =>
            println("Scheduled Vivas" + scheduledVivas)
            assert(scheduledVivas.lengthIs == 2)
            println("Updated Teachers" + updatedTeachers)
            println("Updated Externals" + updatedTeachers)
            assert(totalPreference == 14)
            println("Total Preference" + totalPreference)
          case Left(_) => fail("Expected Right, got Left")
      case _ => fail("Invalid teacher, external, viva, or duration")


  test("scheduleVivas returns Right when valid XML"):
    val xml: Elem = <agenda duration="01:00:00">
      <vivas>
        <viva student="John Doe" title="Thesis Title">
          <president id="T001"/>
          <advisor id="T002"/>
        </viva>
      </vivas>
      <resources>
        <teachers>
          <teacher id="T001" name="Teacher 1">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:30:00" preference="1"/>
          </teacher>
        </teachers>
        <externals>
          <external id="T002" name="External 1">
            <availability start="2007-12-03T09:15:00" end="2007-12-03T11:30:00" preference="1"/>
          </external>
        </externals>
      </resources>
    </agenda>
    val result = Algorithm.scheduleVivas(xml)
    assert(result.isRight)

  test("scheduleVivas returns Left when invalid XML"):
    val xml: Elem = <agenda duration="01:00:00">
      <vivas>
        <viva student="John Doe" title="Thesis Title">
          <president id="1"/>
          <advisor id="2"/>
        </viva>
      </vivas>
      <resources>
        <teachers>
          <teacher id="1">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
          </teacher>
        </teachers>
        <externals>
          <external id="2">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:15:00" preference="1"/>
          </external>
        </externals>
      </resources>
    </agenda>
    val result = Algorithm.scheduleVivas(xml)
    assert(result.isLeft)


  test("updateAgenda should return Right when valid XML is provided") {
    val xml: Elem = <agenda duration="01:00:00">
      <vivas>
        <viva student="John Doe" title="Thesis Title">
          <president id="T001"/>
          <advisor id="T002"/>
        </viva>
      </vivas>
      <resources>
        <teachers>
          <teacher id="T001" name="Teacher 1">
            <availability start="2007-12-03T10:15:00" end="2007-12-03T11:30:00" preference="1"/>
          </teacher>
        </teachers>
        <externals>
          <external id="T002" name="External 1">
            <availability start="2007-12-03T09:15:00" end="2007-12-03T11:30:00" preference="1"/>
          </external>
        </externals>
      </resources>
    </agenda>

    val originalFilePath = "files/test/ms01/valid_file_path.xml"
    val result = Algorithm.updateAgenda(xml, originalFilePath)
    assert(result.isRight)
  }