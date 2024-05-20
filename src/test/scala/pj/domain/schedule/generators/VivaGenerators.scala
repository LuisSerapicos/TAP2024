package pj.domain.schedule.generators

import org.scalacheck.Gen
import pj.domain.Result
import pj.domain.schedule.Algorithm
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, resourceId, vivaStudent, vivaTitle}
import pj.domain.schedule.generators.ResourceGenerators.{generateAdvisor, generateCoadvisor, generatePresident, generateResources, generateSupervisor, generateThreeDigitNumber, generateUniqueResources}
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

  def generateRoles(resources: List[Resource]): Gen[ListMap[resourceId, Role]] =
    for {
      president <- Gen.oneOf(resources)
      advisor <- Gen.oneOf(resources)
      remainingRoles = 5 - 2 // Subtract the number of mandatory roles from the total
      coadvisorCount <- Gen.chooseNum(0, Math.min(3, remainingRoles))
      coadvisors <- Gen.pick(coadvisorCount, resources)
      remainingRoles2 = remainingRoles - coadvisorCount
      supervisorCount <- Gen.chooseNum(0, Math.min(3, remainingRoles2))
      supervisors <- Gen.pick(supervisorCount, resources)
    } yield
      val roles = List(Some(president.id -> Role.President), Some(advisor.id -> Role.Advisor)) ++
        coadvisors.map(coadvisor => Some(coadvisor.id -> Role.Coadvisor)) ++
        supervisors.map(supervisor => Some(supervisor.id -> Role.Supervisor))
      ListMap(roles.flatten *)

  def generateViva(resources: List[Resource]): Gen[Viva] =
    val prefixStudent = Gen.const("Student ")
    val prefixTitle = Gen.const("Title ")
    val uniqueNumber = generateThreeDigitNumber

    for {
      ps <- prefixStudent
      pt <- prefixTitle

      student = vivaStudent.from(ps + Gen.oneOf(uniqueNumber).sample.getOrElse("0"))
      title = vivaTitle.from(pt + Gen.oneOf(uniqueNumber).sample.getOrElse("0"))
      roles <- generateRoles(resources)
      studentF <- student.fold(_ => Gen.fail, student => Gen.const(student))
      titleFinal <- title.fold(_ => Gen.fail, title => Gen.const(title))
    } yield Viva(studentF, titleFinal, roles)


  //Generate vivas, check resources availabilities and assign them to vivas
  def generateInstersectedAvailabilities: Gen[Result[(List[Availability], List[Resource], List[Resource], agendaDuration)]] =
    for {
      agendaDuration <- generateDuration
      teachers <- generateUniqueResources(5, agendaDuration, Set.empty)
      externals <- generateUniqueResources(5, agendaDuration, teachers.map(_.id.to).toSet)
      allResources = teachers ++ externals
      finalResources <- Gen.pick(5, allResources)
      vivas <- generateViva(finalResources.toList)
    } yield
      val scheduledVivasResult = Algorithm.intersectAvailabilities(vivas, teachers, externals, agendaDuration)
      scheduledVivasResult.map(scheduledVivas => (scheduledVivas, teachers, externals, agendaDuration))

  def generateScheduledVivas: Gen[Result[(List[(Viva, Availability)], List[Resource], List[Resource], Int)]] =
    for {
      agendaDuration <- generateDuration
      teachers <- generateUniqueResources(5, agendaDuration, Set.empty)
      externals <- generateUniqueResources(5, agendaDuration, teachers.map(_.id.to).toSet)
      allResources = teachers ++ externals
      finalResources <- Gen.pick(5, allResources)
      vivas <- Gen.listOfN(5, generateViva(finalResources.toList))
    } yield
      val scheduledVivasResult = Algorithm.scheduleAllVivas(vivas, teachers, externals, agendaDuration)
      scheduledVivasResult

