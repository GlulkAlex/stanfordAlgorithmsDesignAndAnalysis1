val elemOpt1: Option[Int] = Some(1)
val elemOpt2: Option[Int] = None
val vector1: Vector[Int] = Vector.empty
val vector2: Vector[Int] = vector1 :+ elemOpt1.get
//val vector3: Vector[Int] = vector2 :+ elemOpt2.orNull
/*val vector3: Vector[Int] =
  vector2 :+ elemOpt2.getOrElse(vector1)*/