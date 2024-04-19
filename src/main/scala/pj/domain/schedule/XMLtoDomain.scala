package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.Domain.*
import pj.domain.schedule.SimpleTypes.{Role, agendaDuration, availabilityEnd, availabilityStart, externalId, externalName, resourceId, resourceName, teacherId, teacherName, vivaId, vivaStudent, vivaTitle}
import pj.io.FileIO
import pj.xml.XML.{fromAttribute, sequence, traverse}

import java.io.PrintWriter
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{Duration, LocalDateTime}
import scala.util.Try
import scala.xml.{Elem, Node, PrettyPrinter}


object XMLtoDomain:
  val xml = FileIO.load("files/test/ms01/simple01.xml")

  def getPreference(xml: Node): Result[Preference] =
    for
      pref <- traverse(xml \ "resources" \ "externals" \ "external" \ "availability", viva => fromAttribute(viva, "preference"))
      prefInt <- pref.headOption match
        case Some(p) => Right(p.toInt)
        case None => Left(DomainError.XMLError("Error"))
      preference <- Preference.from(prefInt.toString)
    yield preference

  val listPref = xml.flatMap(getPreference)

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

  
  def getTeachers(xml: Node): Result[List[Resource]] =
    traverse(xml \ "resources" \ "teachers" \ "teacher", getResource)
  
  def getExternals(xml: Node): Result[List[Resource]] =
    traverse(xml \ "resources" \ "externals" \ "external", getResource)
  
  def getAgendaDuration(xml: Node): Result[agendaDuration] =
    fromAttribute(xml, "duration").flatMap(agendaDuration.from)
  
  def getViva(xml: Node): Result[Viva] =
    for
      student <- fromAttribute(xml, "student")
      studentId <- vivaStudent.from(student)
      title <- fromAttribute(xml, "title")
      titleF <- vivaTitle.from(title)
      roles <- sequence((xml \ "_").map { roleNode =>
        val id = fromAttribute(roleNode, "id").flatMap(resourceId.from)
        val role = roleNode.label match {
          case "president" => Right(Role.President)
          case "advisor" => Right(Role.Advisor)
          case "coadvisor" => Right(Role.Coadvisor)
          case "supervisor" => Right(Role.Supervisor)
          case _ => Left(DomainError.XMLError(s"Invalid role type: ${roleNode.label}"))
        }
        for
          rid <- id
          r <- role
        yield (rid, r)
      }.toList).map(_.toMap)
    yield Viva(studentId, titleF, roles)
  
  def getVivas(xml: Node): Result[List[Viva]] =
    traverse(xml \ "vivas" \ "viva", getViva)
    