package kaos.validation

import kaos.feedback.*
import kaos.model.*

case class ResolutionResult(
    model: ResolvedKaosModel,
    errors: Vector[ValidationError]
)

object ReferenceResolver:

    def resolve(model: KaosModel): ResolutionResult =
        val declarationsById =
            model.elements.groupBy(_.id)

        val resolvedRelationships =
            Vector.newBuilder[ResolvedRelationshipNode]

        val errors =
            Vector.newBuilder[ValidationError]

// Resolve unique IDs to elements
        def resolveElement(
            id: String,
            location: SourceLocation
        ): Option[ElementNode] =
            declarationsById.get(id) match
                case None =>
                    errors += ValidationError(
                      category = ErrorCategory.UnresolvedReference,
                      location = location,
                      message =
                          s"No element with identifier '$id' has been declared."
                    )

                    None

                case Some(declarations) if declarations.size == 1 =>
                    Some(declarations.head)

// Multiple declarations for ID
                case Some(_) =>
                    None

// Create resolved Relationship
        model.relationships.foreach { relationship =>
            val source =
                resolveElement(
                  relationship.sourceId,
                  relationship.location
                )

            val target =
                resolveElement(
                  relationship.targetId,
                  relationship.location
                )

            for
                sourceElement <- source
                targetElement <- target
            do
                resolvedRelationships += ResolvedRelationshipNode(
                  relationshipType = relationship.relationshipType,
                  source = sourceElement,
                  target = targetElement,
                  location = relationship.location
                )
        }

        ResolutionResult(
          model = ResolvedKaosModel(
            elements = model.elements,
            relationships = resolvedRelationships.result()
          ),
          errors = errors.result()
        )
