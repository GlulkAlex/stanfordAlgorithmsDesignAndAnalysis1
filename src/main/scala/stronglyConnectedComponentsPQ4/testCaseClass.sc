import stronglyConnectedComponentsPQ4.stronglyConnectedComponents
.NodeMapValFieldsDynamic

case class IsExplored(var isExplored: Boolean)
val defaultMapValue: IsExplored =
  IsExplored(
                           isExplored = false)
defaultMapValue.isExplored = true
defaultMapValue