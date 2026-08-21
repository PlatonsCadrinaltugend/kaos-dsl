package kaos.validation

import kaos.feedback.*
import kaos.model.*

object Validator:

    def validate(model: KaosModel): Vector[ValidationError] =
        val resolution =
            ReferenceResolver.resolve(model)

        val errors =
            validateDuplicateIdentifiers(model) ++
                resolution.errors ++
                PropertyValidator.validate(model) ++
                validateRelationshipTypes(resolution.model) ++
                SemanticValidator.validate(resolution.model) ++
                HarmonizationValidator.validate(model)

        errors.sortBy(error => (error.location.line, error.location.column))

    private def validateDuplicateIdentifiers(
        model: KaosModel
    ): Vector[ValidationError] =
        model.elements
            .groupBy(_.id)
            .values
            .flatMap { declarations =>
                val sortedDeclarations = declarations.sortBy(element =>
                    (element.location.line, element.location.column)
                )
                val original = sortedDeclarations.head
                sortedDeclarations.drop(1).map { duplicate =>
                    ValidationError(
                      category = ErrorCategory.DuplicateIdentifier,
                      location = duplicate.location,
                      message =
                          s"Identifier '${duplicate.id}' was already declared at line ${original.location.line}."
                    )
                }
            }
            .toVector

    private def validateRelationshipTypes(
        model: ResolvedKaosModel
    ): Vector[ValidationError] =
        model.relationships.flatMap { relationship =>
            val sourceType =
                relationship.source.elementType

            val targetType =
                relationship.target.elementType

            Option.when(
              !isValidRelationship(
                relationship.relationshipType,
                sourceType,
                targetType
              )
            ) {
                ValidationError(
                  category = ErrorCategory.RelationshipType,
                  location = relationship.location,
                  message =
                      s"Relationship '${relationship.relationshipType.phrase}' is not permitted between $sourceType '${relationship.source.id}' and $targetType '${relationship.target.id}'."
                )
            }
        }

    private def isRequisite(
        elementType: ElementType
    ): Boolean =
        elementType match
            case ElementType.Requisite   => true
            case ElementType.Requirement => true
            case ElementType.Assumption  => true
            case _                       => false

    private def isObject(
        elementType: ElementType
    ): Boolean =
        elementType match
            case ElementType.Object => true
            case ElementType.Entity => true
            case ElementType.Event  => true
            case ElementType.Action => true
            case ElementType.Agent  => true
            case _                  => false

    private def isValidRelationship(
        relationshipType: RelationshipType,
        sourceType: ElementType,
        targetType: ElementType
    ): Boolean =
        relationshipType match
            case RelationshipType.Reduces =>
                (
                  sourceType == ElementType.Goal ||
                      isRequisite(sourceType)
                ) &&
                targetType == ElementType.Goal

            case RelationshipType.Conflicts =>
                sourceType == ElementType.Goal &&
                targetType == ElementType.Goal

            case RelationshipType.Concerns =>
                sourceType == ElementType.Goal &&
                isObject(targetType)

            case RelationshipType.Ensures =>
                isObject(sourceType) &&
                isRequisite(targetType)

            case RelationshipType.Operationalizes =>
                sourceType == ElementType.Action &&
                isRequisite(targetType)

            case RelationshipType.Responsibility =>
                sourceType == ElementType.Agent &&
                isRequisite(targetType)

            case RelationshipType.Performs | RelationshipType.Capability =>
                sourceType == ElementType.Agent &&
                targetType == ElementType.Action

            case RelationshipType.Monitors | RelationshipType.Controls =>
                sourceType == ElementType.Agent &&
                isObject(targetType)

            case RelationshipType.Input | RelationshipType.Output =>
                sourceType == ElementType.Action &&
                isObject(targetType)

            case RelationshipType.Obstructs =>
                sourceType == ElementType.Obstacle &&
                (targetType == ElementType.Goal || isRequisite(targetType))

            case RelationshipType.Refines =>
                sourceType == ElementType.Obstacle &&
                targetType == ElementType.Obstacle

            case RelationshipType.Resolves =>
                sourceType == ElementType.Requirement &&
                targetType == ElementType.Obstacle
