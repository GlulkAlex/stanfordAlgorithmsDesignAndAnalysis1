//Some(1) == None
None.contains(1)
val opt1 = None
val opt2 = Some(2)
val opt3 = Some(3)
val opt4 = None
val optList1 = List(opt1,opt2,opt3,opt4)
optList1
//.reduce(_+_)
//.flatMap((x):Option[Int]=>x.get)
.collect({case Some(x)=>x})
  List.empty[Option[Int]]
.collect({case Some(x)=>x})
.headOption