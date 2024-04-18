package pj.domain.schedule

import scala.language.adhocExtensions
import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError

// TODO: Create the code to test a functional domain model for schedule creation.
//       create files in the files/test/ms01 folder
class ScheduleMS01Test extends AnyFunSuite:
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