package kaos.parser

import org.parboiled2.*
import kaos.model.*
enum ParsedDeclaration:
    case Element(value: ElementNode)
    case Relationship(value: RelationshipNode)
class KaosParser(val input: ParserInput) extends Parser:

    def Input: Rule1[KaosModel] = rule:
        WhiteSpace ~
            zeroOrMore(Declaration).separatedBy(RequiredWhiteSpace) ~
            WhiteSpace ~
            EOI ~>
            ((declarations: Seq[ParsedDeclaration]) =>
                KaosModel(
                  elements = declarations.collect {
                      case ParsedDeclaration.Element(element) => element
                  }.toVector,
                  relationships = declarations.collect {
                      case ParsedDeclaration.Relationship(relationship) =>
                          relationship
                  }.toVector
                )
            )

    def ElementDeclaration: Rule1[ElementNode] = rule:
        CurrentOffset ~
            ConceptKeyword ~
            RequiredWhiteSpace ~
            Identifier ~
            optional(WhiteSpace ~ PropertyBlock) ~>
            (
              (
                  offset: Int,
                  elementType: ElementType,
                  id: String,
                  propertyBlock: Option[Vector[PropertyAssignment]]
              ) =>
                  ElementNode(
                    elementType = elementType,
                    id = id,
                    properties = propertyBlock.getOrElse(Vector.empty),
                    location = sourceLocation(offset)
                  )
            )

    def Identifier: Rule1[String] = rule:
        capture(
          CharPredicate.Alpha ~
              zeroOrMore(
                CharPredicate.AlphaNum ++ "_"
              )
        ) ~> ((id: String) => test(!reservedKeywords.contains(id)) ~ push(id))

    def WhiteSpace: Rule0 = rule:
        zeroOrMore(anyOf(" \t\r\n"))

    def RequiredWhiteSpace: Rule0 = rule:
        oneOrMore(anyOf(" \t\r\n"))

    def ConceptKeyword: Rule1[ElementType] = rule:
        ("goal" ~ push(ElementType.Goal)) |
            ("requisite" ~ push(ElementType.Requisite)) |
            ("requirement" ~ push(ElementType.Requirement)) |
            ("assumption" ~ push(ElementType.Assumption)) |
            ("object" ~ push(ElementType.Object)) |
            ("entity" ~ push(ElementType.Entity)) |
            ("event" ~ push(ElementType.Event)) |
            ("action" ~ push(ElementType.Action)) |
            ("agent" ~ push(ElementType.Agent)) |
            ("obstacle" ~ push(ElementType.Obstacle))

    def PropertyBlock: Rule1[Vector[PropertyAssignment]] = rule:
        '{' ~
            WhiteSpace ~
            oneOrMore(PropertyAssignmentRule)
                .separatedBy(WhiteSpace ~ ',' ~ WhiteSpace) ~
            WhiteSpace ~
            '}' ~>
            ((properties: Seq[PropertyAssignment]) => properties.toVector)

    def PropertyAssignmentRule: Rule1[PropertyAssignment] = rule:
        CurrentOffset ~
            PropertyKeyword ~
            WhiteSpace ~
            '=' ~
            WhiteSpace ~
            PropertyValue ~>
            (
              (
                  offset: Int,
                  name: PropertyName,
                  value: String
              ) =>
                  PropertyAssignment(
                    name = name,
                    value = value,
                    location = sourceLocation(offset)
                  )
            )

    def PropertyKeyword: Rule1[PropertyName] = rule:
        ("informalDef" ~ push[PropertyName](PropertyName.InformalDef)) |
            ("formalDef" ~ push[PropertyName](PropertyName.FormalDef)) |
            ("realm" ~ push[PropertyName](PropertyName.Realm))

    def PropertyValue: Rule1[String] = rule:
        '"' ~
            capture(oneOrMore(noneOf("\"\r\n"))) ~
            '"'

    def Declaration: Rule1[ParsedDeclaration] = rule:
        (
          ElementDeclaration ~>
              ((element: ElementNode) => ParsedDeclaration.Element(element))
        ) |
            (
              RelationshipDeclaration ~>
                  ((relationship: RelationshipNode) =>
                      ParsedDeclaration.Relationship(relationship)
                  )
            )

    def RelationshipDeclaration: Rule1[RelationshipNode] = rule:
        CurrentOffset ~
            Identifier ~
            RequiredWhiteSpace ~
            RelationshipPhrase ~
            RequiredWhiteSpace ~
            Identifier ~>
            (
              (
                  offset: Int,
                  sourceId: String,
                  relationshipType: RelationshipType,
                  targetId: String
              ) =>
                  RelationshipNode(
                    relationshipType = relationshipType,
                    sourceId = sourceId,
                    targetId = targetId,
                    location = sourceLocation(offset)
                  )
            )

    def RelationshipPhrase: Rule1[RelationshipType] = rule:
        ("reduces" ~
            push(RelationshipType.Reduces)) |
            ("conflicts" ~
                push(RelationshipType.Conflicts)) |
            ("concerns" ~
                push(RelationshipType.Concerns)) |
            ("ensures" ~
                push(RelationshipType.Ensures)) |
            ("operationalizes" ~
                push(RelationshipType.Operationalizes)) |
            ("is responsible for" ~
                push(RelationshipType.Responsibility)) |
            ("performs" ~
                push(RelationshipType.Performs)) |
            ("is capable of" ~
                push(RelationshipType.Capability)) |
            ("monitors" ~
                push(RelationshipType.Monitors)) |
            ("controls" ~
                push(RelationshipType.Controls)) |
            ("has input" ~
                push(RelationshipType.Input)) |
            ("has output" ~
                push(RelationshipType.Output)) |
            ("obstructs" ~
                push(RelationshipType.Obstructs)) |
            ("resolves" ~
                push(RelationshipType.Resolves)) |
            ("refines" ~
                push(RelationshipType.Refines))

    def CurrentOffset: Rule1[Int] = rule:
        push(cursor)

    private def sourceLocation(offset: Int): SourceLocation =
        var line = 1
        var column = 1
        var index = 0

        while index < offset do
            input.charAt(index) match
                case '\n' =>
                    line += 1
                    column = 1

                case '\r' =>
                    // Bei Windows-Zeilenumbrüchen übernimmt das folgende \n
                    // die Erhöhung der Zeilennummer.
                    if index + 1 >= input.length || input.charAt(
                          index + 1
                        ) != '\n'
                    then line += 1
                    column = 1

                case _ =>
                    column += 1

            index += 1

        SourceLocation(line, column)
    private val reservedKeywords: Set[String] =
        ElementType.values
            .map(_.toString.toLowerCase)
            .toSet ++
            RelationshipType.values
                .map(_.phrase)
                .filterNot(_.contains(" "))
                .toSet ++
            PropertyName.values
                .map(_.syntax)
                .toSet
