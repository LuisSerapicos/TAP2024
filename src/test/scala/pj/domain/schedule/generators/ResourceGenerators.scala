package pj.domain.schedule.generators

import org.scalacheck.Gen
import pj.domain.schedule.Domain.Resource
import pj.domain.schedule.SimpleTypes.{resourceId, resourceName}
import pj.domain.schedule.generators.AvailabilityGenerators.generateAvailability

import scala.util.Random

object ResourceGenerators:

  def generateThreeDigitNumber: Vector[String] =
    val random = new Random()
    val numbers = Random.shuffle((0 to 999).map(n => f"$n%03d")).take(999).toVector // Generate a shuffled sequence of unique 3-digit numbers as strings
    numbers


  def generateResourceId(prefix: String, uniqueNumber: String): Gen[resourceId] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rid = resourceId.from(prefix + uniqueNumber)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId

  def generateResourceName(prefix: String, uniqueNumber: String): Gen[resourceName] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rname = resourceName.from(prefix + uniqueNumber)
      resourceName <- rname.fold(_ => Gen.fail, rname => Gen.const(rname))
    } yield resourceName


  def generatePresident: Gen[resourceId] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rid = resourceId.from("T" + n)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId

  def generateAdvisor: Gen[resourceId] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rid = resourceId.from("T" + n)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId

  def generateCoadvisor: Gen[resourceId] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rid = resourceId.from("T" + n)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId

  def generateSupervisor: Gen[resourceId] =
    for {
      n <- Gen.oneOf(generateThreeDigitNumber)
      rid = resourceId.from("E" + n)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId


  def generateResources: Gen[Resource] =
    for {
      uniqueNumber <- Gen.oneOf(generateThreeDigitNumber)
      prefix <- Gen.oneOf(true, false)
      idPrefix = if (prefix) "T" else "E"
      namePrefix = if (prefix) "Teacher " else "External "
      id <- generateResourceId(idPrefix, uniqueNumber)
      name <- generateResourceName(namePrefix, uniqueNumber)
      availabilities <- Gen.listOfN(5, generateAvailability)
    } yield Resource(id, name, availabilities)

