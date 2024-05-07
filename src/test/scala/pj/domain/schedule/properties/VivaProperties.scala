package pj.domain.schedule.properties

import org.scalacheck.*
import pj.domain.schedule.Domain.{Availability, Resource, Viva}
import pj.domain.schedule.Preference
import pj.domain.schedule.SimpleTypes.{Role, resourceId, vivaStudent, vivaTitle}
import pj.domain.{DomainError, Result}

object VivaProperties extends Properties("VivaProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(10000)

  //Generate sequence
  def generateThreeDigitNumber: Gen[String] =
    val uniqueNumbers = scala.util.Random.shuffle(0 to 999).toList
    val uniqueNumberGen = Gen.oneOf(uniqueNumbers).map(n => f"$n%03d")
    uniqueNumberGen

  def generateResourceId: Gen[resourceId] =
    val prefix = Gen.oneOf("T", "E")
    val uniqueNumber = generateThreeDigitNumber

    for {
      p <- prefix
      n <- uniqueNumber
      rid = resourceId.from(p + n)
      resourceId <- rid.fold(_ => Gen.fail, rid => Gen.const(rid))
    } yield resourceId

  def generateRoles: Gen[Role] = Gen.oneOf(Role.values)

  def generateViva(n: Int): Gen[Viva] =
    val prefixStudent = Gen.const("Student ")
    val prefixTitle = Gen.const("Title ")
    val uniqueNumber = generateThreeDigitNumber

    for {
      ps <- prefixStudent
      pt <- prefixTitle

      student = vivaStudent.from(ps + uniqueNumber.sample.getOrElse("0"))
      title = vivaTitle.from(pt + uniqueNumber.sample.getOrElse("0"))
      roles <- Gen.mapOfN(n, for {
        rid <- generateResourceId
        role <- generateRoles
      } yield (rid, role))
      studentF <- student.fold(_ => Gen.fail, student => Gen.const(student))
      titleFinal <- title.fold(_ => Gen.fail, title => Gen.const(title))
    } yield Viva(studentF, titleFinal, roles)



  property("Generate Roles") = Prop.forAll(generateRoles) { (role: Role) =>
    Role.values.contains(role)
  }

  property("Generate ResourceId") = Prop.forAll(generateResourceId) { (resourceId: resourceId) =>
    resourceId.to.length == 4
  }

  property("Generate vivas") = Prop.forAll(Gen.chooseNum(2, 5)) { (n: Int) =>
    val vivaListGen = Gen.listOfN(n, generateViva(n))
    vivaListGen.sample match
      case Some(vivaList) => vivaList.sizeIs == n
      case None => false
  }
