package kaos.validation

import kaos.feedback.*
import kaos.model.*

object PropertyValidator:

    def validate(model: KaosModel): Vector[ValidationError] =
        model.elements.flatMap(validateElement)

    private def validateElement(
        element: ElementNode
    ): Vector[ValidationError] =
        validateDuplicateProperties(element) ++
            validateRequiredRealm(element) ++
            element.properties.flatMap(property =>
                validateProperty(element, property)
            )
// Check for duplicate properties
    private def validateDuplicateProperties(
        element: ElementNode
    ): Vector[ValidationError] =
        element.properties
            .groupBy(_.name)
            .values
            .flatMap { assignments =>
                val sortedAssignments =
                    assignments.sortBy(assignment =>
                        (
                          assignment.location.line,
                          assignment.location.column
                        )
                    )

                val original = sortedAssignments.head

                sortedAssignments.drop(1).map { duplicate =>
                    ValidationError(
                      category = ErrorCategory.DuplicatePropertyAssignment,
                      location = duplicate.location,
                      message =
                          s"Property '${duplicate.name.syntax}' was already assigned at line ${original.location.line}."
                    )
                }
            }
            .toVector
// Every Agent has to have a realm as property
    private def validateRequiredRealm(
        element: ElementNode
    ): Vector[ValidationError] =
        if element.elementType == ElementType.Agent &&
            !element.properties.exists(_.name == PropertyName.Realm)
        then
            Vector(
              ValidationError(
                category = ErrorCategory.MissingProperty,
                location = element.location,
                message =
                    s"Agent '${element.id}' must define the property 'realm'."
              )
            )
        else Vector.empty
// Properties are only assigned to the elements that allow them
    private def validateProperty(
        element: ElementNode,
        property: PropertyAssignment
    ): Option[ValidationError] =
        if !isPropertyAllowed(
              property.name,
              element.elementType
            )
        then
            Some(
              ValidationError(
                category = ErrorCategory.InvalidPropertyAssignment,
                location = property.location,
                message =
                    s"Property '${property.name.syntax}' cannot be assigned to element type ${element.elementType}."
              )
            )
        else if !isValueAllowed(property) then
            Some(
              ValidationError(
                category = ErrorCategory.InvalidPropertyAssignment,
                location = property.location,
                message =
                    s"Property '${property.name.syntax}' does not permit the value '${property.value}'."
              )
            )
        else None
// Define all Elements that allow formalDef property
    private val formalDefElementTypes =
        Set(
          ElementType.Goal,
          ElementType.Requisite,
          ElementType.Requirement,
          ElementType.Assumption,
          ElementType.Action
        )
// case checks for the allowed properties
    private def isPropertyAllowed(
        propertyName: PropertyName,
        elementType: ElementType
    ): Boolean =
        propertyName match
            case PropertyName.InformalDef =>
                true
            case PropertyName.FormalDef =>
                formalDefElementTypes.contains(elementType)
            case PropertyName.Realm =>
                elementType == ElementType.Agent
// check the property values
    private def isValueAllowed(
        property: PropertyAssignment
    ): Boolean =
        property.name match
            case PropertyName.Realm =>
                property.value == "domain" ||
                property.value == "software"

            case _ =>
                true
