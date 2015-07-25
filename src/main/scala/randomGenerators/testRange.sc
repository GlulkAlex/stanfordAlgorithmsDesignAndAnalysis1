val range1_10 = 1 to 10

range1_10
range1_10.nonEmpty
range1_10.head
range1_10.tail
//range1_10.fold(1: Double)(0.1 * _)
range1_10.fold(1)(_ * _)
range1_10.reduce(_ * _)
//range1_10.fold(1)(2 * _)
scala.math.pow(0.1, 2)
0.1 * 0.1
s"""{\"firstName\":\"John\", \"lastName\":\"${range1_10.head}\"},\n"""
s"""{\"employees\":[\n"""