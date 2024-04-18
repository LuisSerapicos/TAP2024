package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.Domain.*
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, externalId, externalName, resourceId, resourceName, teacherId, teacherName, vivaId, vivaStudent, vivaTitle}
import pj.io.FileIO
import pj.xml.XML.{fromAttribute, traverse}

import java.io.PrintWriter
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Duration, LocalDateTime}
import scala.util.Try
import scala.xml.{Elem, Node, PrettyPrinter}


object XMLtoDomain:
  val xmlTelo = FileIO.load("/home/telogaspar/Documents/Mestrado/TAP/1230196_1231304_ncf/files/test/ms01/simple01.xml")


  def getPreference(xml: Node): Result[Preference] =
    for
      pref <- traverse(xml \ "resources" \ "externals" \ "external" \ "availability", viva => fromAttribute(viva, "preference"))
      prefInt <- pref.headOption match
        case Some(p) => Right(p.toInt)
        case None => Left(DomainError.XMLError("Error"))
      preference <- Preference.from(prefInt.toString)
    yield preference

  val listPref = xmlTelo.flatMap(getPreference)

  def getAvailabilities(node: Node): Result[List[Availability]] =
    traverse(node \ "availability", availability => {
      val start = fromAttribute(availability, "start").map(LocalDateTime.parse)
      val end = fromAttribute(availability, "end").map(LocalDateTime.parse)
      val preference = fromAttribute(availability, "preference").flatMap(preferenceStr =>
        Preference.from(preferenceStr) match {
          case Right(preference) => Right(preference)
          case Left(error) => Left(error)
        }
      )
      for {
        s <- start
        ss <- availabilityStart.from(s)
        e <- end
        ee <- availabilityEnd.from(e)
        p <- preference
      } yield Availability(ss, ee, p)
    })

  def getRoles(node: Node): Result[Map[vivaId, Role]] =
    Right((node \ "vivas" \ "viva").flatMap { roleNode =>
      val idResult = fromAttribute(roleNode, "id").flatMap(vivaId.from)
      val role = roleNode.label match {
        case "president" => Role.President
        case "advisor" => Role.Advisor
        case "coadvisor" => Role.Coadvisor
        case "supervisor" => Role.Supervisor
        case _ => Role.President
      }
      idResult.fold(
        error => {
          println(s"Error parsing vivaId: $error")
          Seq.empty // Return an empty sequence in case of error
        },
        id => Seq((id, role)) // Return a sequence containing the tuple in case of success
      )
    }.toMap)


  def getResource(node: Node): Result[Resource] =
    for
      id <- fromAttribute(node, "id")
      rid <- resourceId.from(id)
      name <- fromAttribute(node, "name")
      rname <- node.label match
        case "teacher" => teacherName.from(name)
        case "external" => externalName.from(name)
        case _ => Left(DomainError.XMLError(s"Invalid resource type: ${node.label}"))
      rnameF <- resourceName.from(rname.toString)
      availabilities <- getAvailabilities(node)
      roles <- getRoles(node)
    yield Resource(rid, rnameF, availabilities, roles)