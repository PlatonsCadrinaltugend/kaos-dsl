package kaos.model

case class SourceLocation(
    line: Int,
    column: Int
)

case class PropertyAssignment(
    name: PropertyName,
    value: String,
    location: SourceLocation
)

case class ElementNode(
    elementType: ElementType,
    id: String,
    properties: Vector[PropertyAssignment],
    location: SourceLocation
)

case class RelationshipNode(
    relationshipType: RelationshipType,
    sourceId: String,
    targetId: String,
    location: SourceLocation
)

case class KaosModel(
    elements: Vector[ElementNode],
    relationships: Vector[RelationshipNode]
)
