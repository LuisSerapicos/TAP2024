package pj.domain.schedule.generators

import org.scalacheck.Gen
import pj.domain.Result
import pj.domain.schedule.Algorithm
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, resourceId, vivaStudent, vivaTitle}
import pj.domain.schedule.generators.ResourceGenerators.{generateThreeDigitNumber, generateUniqueResources}
import pj.domain.schedule.generators.AvailabilityGenerators.generateDuration

import scala.collection.immutable.ListMap

object VivaGenerators:

  def generateStudent: Gen[vivaStudent] =
    val prefix = Gen.const("Student ")

    for {
      p <- prefix
      uniqueNumber <- Gen.oneOf(generateThreeDigitNumber)
      name = p + uniqueNumber
      nameF <- vivaStudent.from(name).fold(_ => Gen.fail, name => Gen.const(name))
    } yield nameF

  def generateTitle: Gen[vivaTitle] =
    val prefix = Gen.const("Title ")

    for {
      p <- prefix
      uniqueNumber <- Gen.oneOf(generateThreeDigitNumber)
      title = p + uniqueNumber
      titleF <- vivaTitle.from(title).fold(_ => Gen.fail, title => Gen.const(title))
    } yield titleF

  def generateRoles(teachers: List[Resource], externals: List[Resource]): Gen[ListMap[resourceId, Role]] =
    val allResources = teachers ++ externals
    for {
      president <- Gen.oneOf(teachers)
      advisor <- Gen.oneOf(teachers.filterNot(_ == president))
      remainingRoles = 3
      coadvisorCount <- Gen.chooseNum(0, Math.min(3, Math.min(remainingRoles, allResources.size - 2))) // Maximum of 3 coadvisors, considering president and advisor are already picked
      coadvisors <- Gen.pick(coadvisorCount, allResources.filterNot(res => res == president || res == advisor))
      remainingRoles2 = remainingRoles - coadvisorCount
      supervisorCount <- Gen.chooseNum(0, Math.min(3, Math.min(remainingRoles2, allResources.size - 2 - coadvisorCount))) // Maximum of 3 supervisors
      supervisors <- Gen.pick(supervisorCount, allResources.filterNot(res => res == president || res == advisor || coadvisors.contains(res)))
    } yield
      val roles = List(Some(president.id -> Role.President), Some(advisor.id -> Role.Advisor)) ++
        coadvisors.map(coadvisor => Some(coadvisor.id -> Role.Coadvisor)) ++
        supervisors.map(supervisor => Some(supervisor.id -> Role.Supervisor))
      ListMap(roles.flatten *)


  def generateViva(teachers: List[Resource], externals: List[Resource]): Gen[Viva] =
    val prefixStudent = Gen.const("Student ")
    val prefixTitle = Gen.const("Title ")
    val uniqueNumber = generateThreeDigitNumber

    for {
      ps <- prefixStudent
      pt <- prefixTitle
      student = vivaStudent.from(ps + Gen.oneOf(uniqueNumber).sample.getOrElse("0"))
      title = vivaTitle.from(pt + Gen.oneOf(uniqueNumber).sample.getOrElse("0"))
      roles <- generateRoles(teachers, externals)
      studentF <- student.fold(_ => Gen.fail, student => Gen.const(student))
      titleFinal <- title.fold(_ => Gen.fail, title => Gen.const(title))
    } yield Viva(studentF, titleFinal, roles)


  //Generate vivas, check resources availabilities and assign them to vivas
  def generateInstersectedAvailabilities: Gen[Result[(List[Availability], List[Resource], List[Resource], agendaDuration)]] =
    for {
      agendaDuration <- generateDuration
      teachers <- generateUniqueResources(5, agendaDuration, Set.empty, true)
      externals <- generateUniqueResources(5, agendaDuration, teachers.map(_.id.to).toSet, false)
      numTeachers <- Gen.chooseNum(2, 5)
      numExternals <- Gen.chooseNum(0, 3)
      teachersF <- Gen.pick(numTeachers, teachers)
      externalsF <- Gen.pick(numExternals, externals)
      vivas <- generateViva(teachersF.toList, externalsF.toList)
    } yield
      val scheduledVivasResult = Algorithm.intersectAvailabilities(vivas, teachers, externals, agendaDuration)
      scheduledVivasResult.map(scheduledVivas => (scheduledVivas, teachers, externals, agendaDuration))

  def generateScheduledVivas: Gen[Result[(List[(Viva, Availability)], List[Resource], List[Resource], Int)]] =
    for {
      agendaDuration <- generateDuration
      teachers <- generateUniqueResources(5, agendaDuration, Set.empty, true)
      externals <- generateUniqueResources(5, agendaDuration, teachers.map(_.id.to).toSet, false)
      numTeachers <- Gen.chooseNum(2, 5)
      numExternals <- Gen.chooseNum(0, 3)
      teachersF <- Gen.pick(numTeachers, teachers)
      externalsF <- Gen.pick(numExternals, externals)
      vivas <- Gen.listOfN(5, generateViva(teachersF.toList, externalsF.toList))
    } yield
      val scheduledVivasResult = Algorithm.scheduleAllVivas(vivas, teachers, externals, agendaDuration)
      scheduledVivasResult

