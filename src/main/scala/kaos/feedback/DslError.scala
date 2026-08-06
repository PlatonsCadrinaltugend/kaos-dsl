package kaos.feedback

import kaos.model.SourceLocation

enum ErrorCategory(val label: String):
    case DuplicateIdentifier extends ErrorCategory("Duplicate identifier error")
    case DuplicateRelationship
        extends ErrorCategory("Duplicate relationship error")
    case DuplicatePropertyAssignment
        extends ErrorCategory("Duplicate property assignment error")
    case UnresolvedReference extends ErrorCategory("Unresolved reference error")
    case RelationshipType extends ErrorCategory("Relationship type error")
    case RelationshipImplication
        extends ErrorCategory("Relationship implication error")
    case InvalidPropertyAssignment
        extends ErrorCategory("Invalid property assignment error")
    case ResponsibilityConstraint
        extends ErrorCategory("Responsibility constraint error")
    case CyclicRelationship extends ErrorCategory("Cyclic Relationship error")
    case MissingProperty extends ErrorCategory("Missing property error")
    case RealmConstraint extends ErrorCategory("Realm constraint error")
    case ConflictingRelationship
        extends ErrorCategory("Conflicting Relationship error")

case class ValidationError(
    category: ErrorCategory,
    location: SourceLocation,
    message: String
):
    override def toString: String =
        s"${category.label} at line ${location.line}, " +
            s"column ${location.column}: $message"
