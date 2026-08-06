package kaos.model

enum ElementType:
    case Goal
    case Requisite
    case Requirement
    case Assumption
    case Object
    case Entity
    case Event
    case Action
    case Agent
    case Obstacle

enum RelationshipType(
    val phrase: String
):
    case Reduces extends RelationshipType("reduces")

    case Conflicts extends RelationshipType("conflicts")

    case Concerns extends RelationshipType("concerns")

    case Ensures extends RelationshipType("ensures")

    case Operationalizes extends RelationshipType("operationalizes")

    case Responsibility extends RelationshipType("is responsible for")

    case Performs extends RelationshipType("performs")

    case Capability extends RelationshipType("is capable of")

    case Monitors extends RelationshipType("monitors")

    case Controls extends RelationshipType("controls")

    case Input extends RelationshipType("has input")

    case Output extends RelationshipType("has output")

enum PropertyName(val syntax: String):
    case InformalDef extends PropertyName("informalDef")
    case FormalDef extends PropertyName("formalDef")
    case Realm extends PropertyName("realm")
