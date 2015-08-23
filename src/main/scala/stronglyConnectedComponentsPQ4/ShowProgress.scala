package stronglyConnectedComponentsPQ4

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date

/**
 * Created by gluk-alex on 8/22/15.
 */
object ShowProgress extends App{

  def convertLongToTimeString(timeNumberMillis: Long):String = {
    val milliSeconds: Long = timeNumberMillis % 1000
    val timeSeconds: Long = timeNumberMillis / 1000
    val seconds: Long = timeSeconds % 60
    val timeMinutes: Long = timeSeconds / 60
    val minutes: Long = timeMinutes % 60
    val timeHours: Long = timeMinutes / 60
    val hours: Long = timeHours % 60
    val timeDays: Long = timeHours / 24
    val days: Long = timeDays % 24

    "days:"+days +"/hours:"+hours +
      "/minutes:" + minutes +"/seconds:" + seconds + "/Millis:" + milliSeconds
  }

  println(
           s"\n`progress` must be shown below:"
         )
  //val today = Calendar.getInstance().getTime()
  // Instantiate a Date object
  val date: Date = new Date()
  // display time and date using toString()
  //System.out.
  println(date.toString())
  printf("%1$s %2$tB %2$td, %2$tY",
         "Due date:",
         date)
  println
  /*sleep for 10 seconds*/
  Thread.sleep(5*60*10)
  val startTime: java.util.Date = Calendar.getInstance().getTime()
  val timeStamp1: Long = System.currentTimeMillis // 1000
  /*Date and Time Pattern */
  val timeStampFormat = new SimpleDateFormat("HH:mm:ss.SSS")
  val timeDifferenceFormat = new SimpleDateFormat("mm:ss.SSS")
  //val minuteFormat = new SimpleDateFormat("mm")
  //val currentMinuteAsString = minuteFormat.format(today)
  val startStampString = timeStampFormat.format(startTime)
  println(s"Start at:" + startStampString)
  println(s"timeStamp1:" + timeStamp1)
  val placeHolderStr: String = "Start[" + " " * 10 + "]"
  //System.out.
  print(placeHolderStr)
  //System.out.flush()
  Thread.sleep(200L)
  (1 to 10)
  .foreach(i => {
    //print(s"\r")
    print(s"Start[${Console.RESET}" +
            s"${Console.YELLOW_B}${Console.RED}>" * i +
            s"${Console.RESET} " * (10 - i) +
            s"${Console.RESET}]\r")
    Thread.sleep(100L)
  })
  val endTime = Calendar.getInstance().getTime()
  val timeStamp2: Long = System.currentTimeMillis() // 1000
  val endStampString = timeStampFormat.format(startTime)
  val timeDifference =
    /*timeDifferenceFormat
    .format(*/
        endTime.after(startTime)
           //)
  println
  println(s"Done at:" + endStampString)
  println(s"timeStamp2:" + timeStamp2)
  println(s"'endTime.after(startTime)' is:" + timeDifference)
  println(s"time difference is:" + (timeStamp2 - timeStamp1) + " Millis")
  println(s"time difference is:" +
            //printf("%tM:%tS.%tL",date,(timeStamp2 - timeStamp1))
            convertLongToTimeString(
                                     timeNumberMillis =
                                       (timeStamp2 - timeStamp1))
         )
  /*println(s"time difference is:" +
    String
    .format("%mm:%ss.%SSS",(timeStamp2 - timeStamp1).toString))*/

  val placeHolderStr2: String = "Start[" + "X" * 10 + "]"
  print(placeHolderStr2)
  Thread.sleep(200L)
  (1 to 10)
  .foreach(i => {
    print(
            //s"${Console.YELLOW_B}${Console.RED}\b${Console.RESET}")
            s"${Console.YELLOW_B}${Console.RED}\t${Console.RESET}")
            //s"${Console.YELLOW_B}${Console.RED}\f${Console.RESET}")
            //s"${Console.YELLOW_B}${Console.RED}\010${Console.RESET}")
            //s"${Console.YELLOW_B}${Console.RED}\\b${Console.RESET}")
    Thread.sleep(200L)
  })
  println
  (1 to 40)
  .foreach(i => {
    //print(s"$i\r")
    //print(s".")
    //System.out.print("\33[1A\33[2K")
    //print(" \u0008")
    //print("." * i + "\r")
    //Console.flush()
    /* spins spinner*/
    print(s"\r")
    (i % 4) match {
      case 0 => {
        print(s"${Console.YELLOW}\\\r")}
      case 1 => {
        print(s"${Console.BLUE}|\r")}
      case 2 => {
        print(s"${Console.RED}/\r")}
      case 3 => {
        print(s"${Console.GREEN}--\r")}
    }
    //Console.flush()
    Thread.sleep(100L)
  })
}
