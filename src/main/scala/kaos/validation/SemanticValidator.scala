package kaos.validation

import kaos.feedback.*
import kaos.model.*

object SemanticValidator:

    def validate(
        model: ResolvedKaosModel
    ): Vector[ValidationError] =
        validateResponsibilities(model) ++
            validateResponsibilityRealms(model)

    private def validateResponsibilities(
        model: ResolvedKaosModel
    ): Vector[ValidationError] =
        model.relationships
            .filter(
              _.relationshipType == RelationshipType.Responsibility
            )
            .groupBy(_.target.id)
            .values
            .flatMap { responsibilities =>
                val sorted =
                    responsibilities.sortBy(relationship =>
                        (
                          relationship.location.line,
                          relationship.location.column
                        )
                    )

                val distinctAgents =
                    sorted
                        .groupBy(_.source.id)
                        .values
                        .map(_.head)
                        .toVector
                        .sortBy(relationship =>
                            (
                              relationship.location.line,
                              relationship.location.column
                            )
                        )

                distinctAgents.headOption.toVector.flatMap { original =>
                    distinctAgents.drop(1).map { conflicting =>
                        ValidationError(
                          category = ErrorCategory.ResponsibilityConstraint,
                          location = conflicting.location,
                          message =
                              s"${conflicting.target.elementType} '${conflicting.target.id}' is already assigned to Agent '${original.source.id}' at line ${original.location.line}."
                        )
                    }
                }
            }
            .toVector

    private def validateResponsibilityRealms(
        model: ResolvedKaosModel
    ): Vector[ValidationError] =
        model.relationships
            .filter(
              _.relationshipType == RelationshipType.Responsibility
            )
            .flatMap { relationship =>
                val requiredRealm =
                    relationship.target.elementType match
                        case ElementType.Requirement =>
                            Some("software")

                        case ElementType.Assumption =>
                            Some("domain")

                        case _ =>
                            None

                val actualRealm =
                    relationship.source.properties
                        .find(_.name == PropertyName.Realm)
                        .map(_.value)

                requiredRealm match
                    case Some(expectedRealm)
                        if actualRealm.exists(_ != expectedRealm) =>
                        Some(
                          ValidationError(
                            category = ErrorCategory.RealmConstraint,
                            location = relationship.location,
                            message =
                                s"${relationship.target.elementType} '${relationship.target.id}' must be assigned to an Agent with realm '$expectedRealm', but Agent '${relationship.source.id}' has realm '${actualRealm.get}'."
                          )
                        )

                    case _ =>
                        None
            }
