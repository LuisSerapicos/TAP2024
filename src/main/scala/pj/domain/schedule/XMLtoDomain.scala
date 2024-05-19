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
  val xml2 = FileIO.load("C:\\Users\\Luis Serapicos\\Desktop\\1230196_1231304_ncf\\files\\test\\ms01\\simple01.xml")


  /**
   * Converts a string representation of a duration to a Duration object.
   *
   * The string should be in the format "HH:MM:SS", where HH represents hours, MM represents minutes, and SS represents seconds.
   *
   * @param durationString The string representation of the duration.
   * @return An Option[Duration] which is a Some[Duration] if the conversion is successful, or a None if an error occurs during the conversion.
   */
  def getPreference(xml: Node): Result[Preference] =
    for
      pref <- traverse((xml \ "resources" \ "teachers" \ "teacher" \ "availability") ++ (xml \ "resources" \ "externals" \ "external" \ "availability"), viva => fromAttribute(viva, "preference"))
      prefInt <- pref.headOption match
        case Some(p) => Right(p.toInt)
        case None => Left(DomainError.XMLError("Error"))
      preference <- Preference.from(prefInt.toString)
    yield preference

  val listPref = xml2.flatMap(getPreference)

  /**
   * Extracts the availabilities from the XML node.
   *
   * @param node The XML node.
   * @return A Result[List[Availability]] which is a Right[List[Availability]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
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

  /**
   * Extracts the roles from the XML node.
   *
   * @param node The XML node.
   * @return A Result[Map[resourceId, Role]] which is a Right[Map[resourceId, Role]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
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
  
  /**
   * Extracts the resource from the XML node.
   *
   * @param node The XML node.
   * @return A Result[Resource] which is a Right[Resource] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
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

  /**
   * Extracts the teachers from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[List[Resource]] which is a Right[List[Resource]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getTeachers(xml: Node): Result[List[Resource]] =
    xml \ "resources" \ "teachers" \ "teacher" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No teacher nodes found"))
      case nodes => traverse(nodes, getResource)

  /**
   * Extracts the externals from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[List[Resource]] which is a Right[List[Resource]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getExternals(xml: Node): Result[List[Resource]] =
    xml \ "resources" \ "externals" \ "external" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No external nodes found"))
      case nodes => traverse(nodes, getResource)

  /**
   * Extracts the agenda duration from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[agendaDuration] which is a Right[agendaDuration] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getAgendaDuration(xml: Node): Result[agendaDuration] =
    fromAttribute(xml, "duration").flatMap(agendaDuration.from)

  /**
   * Extracts the viva from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[Viva] which is a Right[Viva] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getViva(xml: Node): Result[Viva] =
    for
      student <- fromAttribute(xml, "student")
      studentId <- vivaStudent.from(student)
      title <- fromAttribute(xml, "title")
      titleF <- vivaTitle.from(title)
      roles <- getRoles(xml)
    yield Viva(studentId, titleF, roles)


  /**
   * Extracts the vivas from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[List[Viva]] which is a Right[List[Viva]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getVivas(xml: Node): Result[List[Viva]] =
    xml \ "vivas" \ "viva" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No viva nodes found"))
      case nodes => traverse(nodes, getViva)
      
      