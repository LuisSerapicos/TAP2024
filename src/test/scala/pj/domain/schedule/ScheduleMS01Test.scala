package pj.domain.schedule

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError
import pj.domain.schedule.SimpleTypes.{agendaDuration, availabilityEnd, availabilityStart, externalId, resourceId, teacherId, vivaStudent, vivaTitle}
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
    val result = agendaDuration.from("12:30:00")
    assert(result.isRight)

  test("agendaDuration from should return Left when invalid duration"):
    val result = agendaDuration.from("25:00:00")
    assert(result.isLeft)

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
    println(s"Result 1: $result1")
    assert(result1.isRight)

  test("availabilityStart from should return Left when invalid start"):
    val result2 = availabilityStart.from(LocalDateTime.parse("2007-12-03T10:15"))
    println(s"Result 2: $result2")
    assert(result2.isLeft)

  test("availabilityEnd from should return Right when valid end"):
    val result = availabilityEnd.from(LocalDateTime.parse("2007-12-03T10:15:00"))
    assert(result.isRight)

  test("availabilityEnd from should return Left when invalid end"):
    val result = availabilityEnd.from(LocalDateTime.parse("2007-12-03T10:15"))
    assert(result.isLeft)

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