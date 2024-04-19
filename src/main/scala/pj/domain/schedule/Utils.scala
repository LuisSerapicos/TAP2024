package pj.domain.schedule

import java.time.Duration
import scala.util.Try

object Utils:
  /**
   * Converts a string representation of a duration to a Duration object.
   *
   * The string should be in the format "HH:MM:SS", where HH represents hours, MM represents minutes, and SS represents seconds.
   *
   * @param durationString The string representation of the duration.
   * @return An Option[Duration] which is a Some[Duration] if the conversion is successful, or a None if an error occurs during the conversion.
   */
  def stringToDuration(durationString: String): Option[Duration] =
    Try(Duration.ofHours(durationString.split(":")(0).toLong)
      .plusMinutes(durationString.split(":")(1).toLong)
      .plusSeconds(durationString.split(":")(2).toLong)
    ).toOption
