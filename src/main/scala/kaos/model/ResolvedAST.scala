package kaos.model

case class ResolvedRelationshipNode(
    relationshipType: RelationshipType,
    source: ElementNode,
    target: ElementNode,
    location: SourceLocation
)

case class ResolvedKaosModel(
    elements: Vector[ElementNode],
    relationships: Vector[ResolvedRelationshipNode]
)
