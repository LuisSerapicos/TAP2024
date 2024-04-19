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
  val xml2 = FileIO.load("files/test/ms01/simple01.xml")


  def getPreference(xml: Node): Result[Preference] =
    for
      pref <- traverse((xml \ "resources" \ "teachers" \ "teacher" \ "availability") ++ (xml \ "resources" \ "externals" \ "external" \ "availability"), viva => fromAttribute(viva, "preference"))
      prefInt <- pref.headOption match
        case Some(p) => Right(p.toInt)
        case None => Left(DomainError.XMLError("Error"))
      preference <- Preference.from(prefInt.toString)
    yield preference

  val listPref = xml2.flatMap(getPreference)

  def getAvailabilities(node: Node): Result[List[Availability]] =
    val availabilities = traverse(node \ "availability", availability => {
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

    availabilities.flatMap:
      case list if list.isEmpty => Left(DomainError.XMLError("No availability nodes found"))
      case list => Right(list)

  def getRoles(node: Node): Result[Map[resourceId, Role]] =
    val roles = (node \ "_").flatMap { roleNode =>
      val idResult = fromAttribute(roleNode, "id").flatMap(resourceId.from)
      val role = roleNode.label match
        case "president" => Role.President
        case "advisor" => Role.Advisor
        case "coadvisor" => Role.Coadvisor
        case "supervisor" => Role.Supervisor
        case _ => Role.President
      idResult.fold(
        error => Seq.empty,
        id => Seq((id, role))
      )
    }.toMap
    roles.sizeIs match
      case s if s < 2 => Left(DomainError.InvalidNumberOfRoles("Required number of roles is minimum of 2"))
      case _ => Right(roles)


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
    yield Resource(rid, rnameF, availabilities)

  def getTeachers(xml: Node): Result[List[Resource]] =
    xml \ "resources" \ "teachers" \ "teacher" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No teacher nodes found"))
      case nodes => traverse(nodes, getResource)

  def getExternals(xml: Node): Result[List[Resource]] =
    xml \ "resources" \ "externals" \ "external" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No external nodes found"))
      case nodes => traverse(nodes, getResource)

  def getAgendaDuration(xml: Node): Result[agendaDuration] =
    fromAttribute(xml, "duration").flatMap(agendaDuration.from)

  def getViva(xml: Node): Result[Viva] =
    for
      student <- fromAttribute(xml, "student")
      studentId <- vivaStudent.from(student)
      title <- fromAttribute(xml, "title")
      titleF <- vivaTitle.from(title)
      roles <- getRoles(xml)
    yield Viva(studentId, titleF, roles)

  def getVivas(xml: Node): Result[List[Viva]] =
    xml \ "vivas" \ "viva" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No viva nodes found"))
      case nodes => traverse(nodes, getViva)
      
      