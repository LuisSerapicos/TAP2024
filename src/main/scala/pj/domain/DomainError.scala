package pj.domain

type Result[A] = Either[DomainError,A]

enum DomainError:
  case IOFileProblem(error: String)
  case XMLError(error: String)
  
  case InvalidAgendaDuration(duration: String)
  case InvalidVivaStudent(studentId: String)
  case InvalidVivaTitle(title: String)
  case InvalidPresidentId(presidentId: String)
  case InvalidAdvisorId(advisorId: String)
  case InvalidSupervisorId(supervisorId: String)
  case InvalidTeacherId(teacherId: String)
  case InvalidTeacherName(teacherName: String)
  case InvalidAvailabilityStart(start: String)
  case InvalidAvailabilityEnd(end: String)
  case InvalidExternalId(externalId: String)
  case InvalidExternalName(externalName: String)
  case InvalidPreference(pref: String)
  case InvalidVivaId(vivaId: String)
  case InvalidResourceId(resourceId: String)
  case InvalidResourceName(resourceName: String)
  case InvalidNumberOfRoles(roles: String)
  case ImpossibleSchedule
  case ImpossibleSchedule2(s:String)

  case InvalidAvailabilityDate(date: String)