package pj.domain.schedule.ScheduleMS03Tests

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError

class DomainErrorTests extends AnyFunSuite:
  //DomainError Tests
  test("ImpossibleSchedule should have error message"):
    val impossibleSchedule = DomainError.ImpossibleSchedule
    assert(impossibleSchedule.toString == "ImpossibleSchedule")

  test("InvalidAvailabilityDate should have error message"):
    val invalidAvailabilityDate = DomainError.InvalidAvailabilityDate
    assert(invalidAvailabilityDate.toString == "InvalidAvailabilityDate")

  test("InvalidAgendaDuration should have error message"):
    val invalidAgendaDuration = DomainError.InvalidAgendaDuration
    assert(invalidAgendaDuration.toString == "InvalidAgendaDuration")

  test("InvalidNumberOfRoles should have error message"):
    val invalidNumberOfRoles = DomainError.InvalidNumberOfRoles
    assert(invalidNumberOfRoles.toString == "InvalidNumberOfRoles")
