package pj.domain.schedule.generators

import org.scalacheck.Gen
import pj.domain.schedule.Domain.Resource
import pj.domain.schedule.SimpleTypes.{agendaDuration, resourceId, resourceName}
import pj.domain.schedule.generators.AvailabilityGenerators.generateAvailability

import scala.util.Random

object ResourceGenerators:

  def generateThreeDigitNumber: Vector[String] =
    val numbers = Random.shuffle((1 to 999).map(n => f"$n%03d")).take(999).toVector // Generate a shuffled sequence of unique 3-digit numbers as strings
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


  def generateUniqueResource(generatedIds: Set[String], duration: agendaDuration, isTeacher: Boolean): Gen[Resource] = {
    for {
      uniqueNumber <- Gen.oneOf(generateThreeDigitNumber)
      idPrefix = if (isTeacher) "T" else "E"
      namePrefix = if (isTeacher) "Teacher " else "External "
      id <- generateResourceId(idPrefix, uniqueNumber)
      name <- generateResourceName(namePrefix, uniqueNumber)
      availabilities <- Gen.listOfN(10, generateAvailability(duration))
    } yield Resource(id, name, availabilities)
  }.suchThat(resource => !generatedIds.contains(resource.id.to))

  def generateUniqueResources(n: Int, duration: agendaDuration, generatedIds: Set[String], isTeacher: Boolean): Gen[List[Resource]] =
    if (n <= 0)
      Gen.const(List.empty[Resource])
    else
      for {
        resource <- generateUniqueResource(generatedIds, duration, isTeacher)
        resources <- generateUniqueResources(n - 1, duration, generatedIds + resource.id.to, isTeacher)
      } yield resource :: resources
      
      