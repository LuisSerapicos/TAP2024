package pj.domain.schedule

import pj.domain.{DomainError, Result}
import pj.domain.schedule.SimpleTypes.*
import pj.domain.schedule.Domain.{Availability2, Resource, Role2, Viva, VivaDuration}
import pj.io.FileIO
import pj.xml.XML.{fromAttribute, fromNode, traverse}

import scala.xml.Node


object XMLtoDomain:

  val xml2 = FileIO.load("C:\\Users\\Luis Serapicos\\Desktop\\1230196_1231304_ncf\\files\\test\\ms01\\simple01.xml")


  /**
   * Extracts the preference from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[Preference] which is a Right[Preference] if the operation is successful, or a Left[DomainError] if an error occurs.
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
   * @return A Result[Availability2] which is a Right[Availability2] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getAvailabilities(node: Node): Result[Availability2] =
    for
      start <- fromAttribute(node, "start")
      startF <- availabilityDate.from(start)
      end <- fromAttribute(node, "end")
      endF <- availabilityDate.from(end)
      preference <- fromAttribute(node, "preference")
      preferenceF <- Preference.from(preference)
    yield Availability2(startF, endF, preferenceF)

  /**
   * Extracts the roles from the XML node.
   *
   * @param node       The XML node.
   * @param resourceId The resource id.
   * @return A Result[Role2] which is a Right[Role2] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getRoles(node: Node, resourceId: resourceId): Result[Role2] =
    fromNode(node, "president").flatMap { presidentNode =>
      fromAttribute(presidentNode, "id").flatMap { presidentId =>
        if (presidentId == resourceId.to) Right(Role2.President)
        else fromNode(node, "advisor").flatMap { advisorNode =>
          fromAttribute(advisorNode, "id").flatMap { advisorId =>
            if (advisorId == resourceId.to) Right(Role2.Advisor)
            else traverse((node \ "coadvisor").toList, coAdvisor => fromAttribute(coAdvisor, "id")).flatMap { coAdvisorIds =>
              if (coAdvisorIds.contains(resourceId.to)) Right(Role2.Coadvisor)
              else traverse((node \ "supervisor").toList, supervisor => fromAttribute(supervisor, "id")).flatMap { supervisorIds =>
                if (supervisorIds.contains(resourceId.to)) Right(Role2.Supervisor)
                else Right(Role2.None)
              }
            }
          }
        }
      }
    }

  /**
   * Extracts the resource from the XML node.
   *
   * @param node The XML node.
   * @return A Result[(resourceId, resourceName)] which is a Right[(resourceId, resourceName)] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getResource(node: Node): Result[(resourceId, resourceName)] =
    for
      id <- fromAttribute(node, "id")
      idF <- resourceId.from(id)
      name <- fromAttribute(node, "name")
      nameF <- resourceName.from(name)
    yield (idF, nameF)

  /**
   * Extracts the teachers from the XML node.
   *
   * @param xml      The XML node.
   * @param vivaNode The viva XML node.
   * @return A Result[List[Resource]] which is a Right[List[Resource]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getTeachers(xml: Node, vivaNode: Node): Result[List[Resource]] =
    traverse((xml \\ "teacher"), teacher => {
      for
        (id, name) <- getResource(teacher)
        availabilities <- traverse((teacher \ "availability"), getAvailabilities)
        role <- getRoles(vivaNode, id)
      yield role match {
        case Role2.None => None
        case _ => Some(Resource(id, name, availabilities, role))
      }
    }).map(_.flatten)

  /**
   * Extracts the externals from the XML node.
   *
   * @param xml      The XML node.
   * @param vivaNode The viva XML node.
   * @return A Result[List[Resource]] which is a Right[List[Resource]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getExternals(xml: Node, vivaNode: Node): Result[List[Resource]] =
    traverse((xml \\ "external"), external => {
      for
        (id, name) <- getResource(external)
        availabilities <- traverse((external \ "availability"), getAvailabilities)
        role <- getRoles(vivaNode, id)
      yield role match {
        case Role2.None => None
        case _ => Some(Resource(id, name, availabilities, role))
      }
    }).map(_.flatten)

  /**
   * Extracts the agenda duration from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[VivaDuration] which is a Right[VivaDuration] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getVivaDuration(xml: Node): Result[VivaDuration] =
    for
      duration <- fromAttribute(xml, "duration").flatMap(agendaDuration.from)
      durationF <- VivaDuration.create(duration)
    yield durationF

  /**
   * Extracts the viva from the XML node.
   *
   * @param vivaNode The XML node.
   * @param xml      The XML document node.
   * @return A Result[Viva] which is a Right[Viva] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getViva(vivaNode: Node, xml: Node): Result[Viva] =
    for {
      student <- fromAttribute(vivaNode, "student")
      studentF <- vivaStudent.from(student)
      title <- fromAttribute(vivaNode, "title")
      titleF <- vivaTitle.from(title)
      resources1 <- getExternals(xml, vivaNode)
      resources2 <- getTeachers(xml, vivaNode)
    } yield Viva(studentF, titleF, resources2 ++ resources1)

  /**
   * Extracts the vivas from the XML node.
   *
   * @param xml The XML node.
   * @return A Result[List[Viva]] which is a Right[List[Viva]] if the operation is successful, or a Left[DomainError] if an error occurs.
   */
  def getVivas(xml: Node): Result[List[Viva]] =
    xml \ "vivas" \ "viva" match
      case nodes if nodes.isEmpty => Left(DomainError.XMLError("No viva nodes found"))
      case nodes => traverse(nodes, vivaNode => getViva(vivaNode, xml))