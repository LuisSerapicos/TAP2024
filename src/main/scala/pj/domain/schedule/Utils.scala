package pj.domain.schedule

import java.time.Duration
import scala.util.Try

object Utils:
  def stringToDuration(durationString: String): Option[Duration] =
    Try(Duration.ofHours(durationString.split(":")(0).toLong)
      .plusMinutes(durationString.split(":")(1).toLong)
      .plusSeconds(durationString.split(":")(2).toLong)
    ).toOption
