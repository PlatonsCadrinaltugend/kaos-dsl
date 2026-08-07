import kaos.feedback.*
import kaos.model.*
import kaos.parser.KaosParser
import kaos.validation.Validator

class KaosDslSuite extends munit.FunSuite:

    private def parse(input: String): KaosModel =
        val parser = new KaosParser(input)
        parser.Input.run().get

    private def validate(input: String): Vector[ValidationError] =
        Validator.validate(parse(input))

    private def parsingFails(input: String): Boolean =
        val parser = new KaosParser(input)
        parser.Input.run().isFailure
// testing basic functionality
    test("accepts a valid model") {

        val errors =
            validate(
              """
                |agent a1 {
                |    realm = "software"
                |}
                |
                |requirement req1
                |action action1
                |
                |a1 is responsible for req1
                |a1 performs action1
                |a1 is capable of action1
                |""".stripMargin
            )

        assertEquals(errors, Vector.empty)
    }
    test("accepts an empty model") {
        val model = parse("")

        assertEquals(model.elements, Vector.empty)
        assertEquals(model.relationships, Vector.empty)
    }
    test("supports forward references") {
        val errors =
            validate(
              """
                |a1 performs action1
                |a1 is capable of action1
                |
                |agent a1 {
                |    realm = "software"
                |}
                |
                |action action1
                |""".stripMargin
            )

        assertEquals(
          errors,
          Vector.empty
        )
    }
    test("parses Unicode characters in property values") {
        val model =
            parse(
              """
                |goal improveHealth {
                |    informalDef = "Öffentliche Gesundheit verbessern."
                |}
                |""".stripMargin
            )

        assertEquals(
          model.elements.head.properties.head.value,
          "Öffentliche Gesundheit verbessern."
        )
    }
    test("rejects unknown concept keywords") {
        assert(
          parsingFails("softgoal goal1")
        )
    }
    test("rejects unknown relationship phrases") {
        assert(
          parsingFails(
            """
                |goal goal1
                |goal goal2
                |goal1 supports goal2
                |""".stripMargin
          )
        )
    }
    test("rejects missing element identifiers") {
        assert(
          parsingFails("goal")
        )
    }
    test("rejects identifiers starting with a digit") {
        assert(
          parsingFails("goal 1goal")
        )
    }
    test("accepts whitespace between declarations") {
        val errors =
            validate(
              """

                |goal goal1


                |goal goal2

                |goal1 conflicts goal2

                |""".stripMargin
            )

        assertEquals(errors, Vector.empty)
    }
// testing elements
    test("parses an element declaration") {
        val model =
            parse(
              """
                |goal improveKnowledge
                |""".stripMargin
            )

        assertEquals(model.elements.size, 1)
        assertEquals(
          model.elements.head.elementType,
          ElementType.Goal
        )
        assertEquals(
          model.elements.head.id,
          "improveKnowledge"
        )
    }
    test("parses all supported element types") {
        val cases =
            Vector(
              "goal" -> ElementType.Goal,
              "requisite" -> ElementType.Requisite,
              "requirement" -> ElementType.Requirement,
              "assumption" -> ElementType.Assumption,
              "object" -> ElementType.Object,
              "entity" -> ElementType.Entity,
              "event" -> ElementType.Event,
              "action" -> ElementType.Action,
              "agent" -> ElementType.Agent,
              "obstacle" -> ElementType.Obstacle
            )

        cases.foreach { case (keyword, expectedType) =>
            val model =
                parse(s"$keyword element1")

            assertEquals(
              model.elements.head.elementType,
              expectedType
            )
        }
    }
    test("reports a duplicate identifier") {
        val errors =
            validate(
              """
                |goal goal1
                |requirement goal1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.DuplicateIdentifier
          )
        )
    }
    test("reports an unresolved source reference") {
        val errors =
            validate(
              """
                |action action1
                |missingAgent performs action1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.UnresolvedReference
          )
        )
    }
    test("reports an unresolved target reference") {
        val errors =
            validate(
              """
                |agent a1 {
                |    realm = "domain"
                |}
                |a1 performs missingAction
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.UnresolvedReference
          )
        )
    }
// testing relationships
    test("parses a relationship") {

        val model =
            parse(
              """
                |agent a1 {
                |    realm = "domain"
                |}
                |action provideInformation
                |a1 performs provideInformation
                |""".stripMargin
            )

        assertEquals(model.relationships.size, 1)
        assertEquals(
          model.relationships.head.relationshipType,
          RelationshipType.Performs
        )
    }
    test("parses and validates all supported relationships") {
        val model =
            parse(
              """
                |goal goal1
                |goal goal2
                |requirement req1
                |requirement req2
                |object object1
                |action action1
                |
                |agent a1 {
                |    realm = "software"
                |}
                |
                |req1 reduces goal1
                |goal1 conflicts goal2
                |goal1 concerns object1
                |object1 ensures req1
                |action1 operationalizes req1
                |a1 is responsible for req2
                |a1 performs action1
                |a1 is capable of action1
                |a1 monitors object1
                |a1 controls object1
                |action1 has input object1
                |action1 has output object1
                |""".stripMargin
            )

        assertEquals(
          model.relationships.map(_.relationshipType),
          Vector(
            RelationshipType.Reduces,
            RelationshipType.Conflicts,
            RelationshipType.Concerns,
            RelationshipType.Ensures,
            RelationshipType.Operationalizes,
            RelationshipType.Responsibility,
            RelationshipType.Performs,
            RelationshipType.Capability,
            RelationshipType.Monitors,
            RelationshipType.Controls,
            RelationshipType.Input,
            RelationshipType.Output
          )
        )

        assertEquals(
          Validator.validate(model),
          Vector.empty
        )
    }
    test("reports a duplicate relationship") {
        val errors =
            validate(
              """
                |goal goal1
                |goal goal2
                |goal1 conflicts goal2
                |goal1 conflicts goal2
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.DuplicateRelationship
          )
        )
    }
    test("reports responsibility constraint error") {
        val errors =
            validate(
              """
                |requirement req1
                |
                |agent a1 {
                |    realm = "software"
                |}
                |
                |agent a2 {
                |    realm = "software"
                |}
                |
                |a1 is responsible for req1
                |a2 is responsible for req1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.ResponsibilityConstraint
          )
        )
    }
    test("reports invalid endpoint types for all relationships") {
        val cases =
            Vector(
              (
                "reduces",
                """
                    |agent a1 {
                    |    realm = "software"
                    |}
                    |goal goal1
                    |a1 reduces goal1
                    |""".stripMargin
              ),
              (
                "conflicts",
                """
                    |agent a1 {
                    |    realm = "software"
                    |}
                    |goal goal1
                    |a1 conflicts goal1
                    |""".stripMargin
              ),
              (
                "concerns",
                """
                    |agent a1 {
                    |    realm = "software"
                    |}
                    |object object1
                    |a1 concerns object1
                    |""".stripMargin
              ),
              (
                "ensures",
                """
                    |goal goal1
                    |requirement req1
                    |goal1 ensures req1
                    |""".stripMargin
              ),
              (
                "operationalizes",
                """
                    |goal goal1
                    |requirement req1
                    |goal1 operationalizes req1
                    |""".stripMargin
              ),
              (
                "is responsible for",
                """
                    |goal goal1
                    |requirement req1
                    |goal1 is responsible for req1
                    |""".stripMargin
              ),
              (
                "performs",
                """
                    |goal goal1
                    |action action1
                    |goal1 performs action1
                    |""".stripMargin
              ),
              (
                "is capable of",
                """
                    |goal goal1
                    |action action1
                    |goal1 is capable of action1
                    |""".stripMargin
              ),
              (
                "monitors",
                """
                    |goal goal1
                    |object object1
                    |goal1 monitors object1
                    |""".stripMargin
              ),
              (
                "controls",
                """
                    |goal goal1
                    |object object1
                    |goal1 controls object1
                    |""".stripMargin
              ),
              (
                "has input",
                """
                    |agent a1 {
                    |    realm = "software"
                    |}
                    |object object1
                    |a1 has input object1
                    |""".stripMargin
              ),
              (
                "has output",
                """
                    |agent a1 {
                    |    realm = "software"
                    |}
                    |object object1
                    |a1 has output object1
                    |""".stripMargin
              )
            )

        cases.foreach { case (relationshipName, input) =>
            val errors =
                validate(input)

            assert(
              errors.exists(
                _.category == ErrorCategory.RelationshipType
              ),
              s"Expected a relationship type error for '$relationshipName'."
            )
        }
    }
    test("reports relationship type error for invalid target") {
        val errors =
            validate(
              """
                |agent a1 {
                |    realm = "software"
                |}
                |goal goal1
                |
                |a1 performs goal1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.RelationshipType
          )
        )
    }
// testing properties
    test("parses properties") {
        val model =
            parse(
              """
                    |agent a1 {
                    |    realm = "domain",
                    |    informalDef = "A system user"
                    |}
                    |""".stripMargin
            )

        val agent = model.elements.head

        assertEquals(agent.properties.size, 2)
        assertEquals(
          agent.properties.map(_.name).toSet,
          Set(
            PropertyName.Realm,
            PropertyName.InformalDef
          )
        )
    }
    test("reports an invalid property assignment") {
        val errors =
            validate(
              """
                |agent a1 {
                |    formalDef = "invalid"
                |}
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.InvalidPropertyAssignment
          )
        )
    }
    test("reports an invalid realm value") {
        val errors =
            validate(
              """
                |agent a1 {
                |    realm = "human"
                |}
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.InvalidPropertyAssignment
          )
        )
    }
    test("reports missing property error") {
        val errors =
            validate(
              """
                |agent a1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.MissingProperty
          )
        )
    }
    test("reports a duplicate property assignment") {
        val errors =
            validate(
              """
                |agent a1 {
                |    realm = "domain",
                |    realm = "software"
                |}
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.DuplicatePropertyAssignment
          )
        )
    }
    test("rejects unknown property names") {
        assert(
          parsingFails(
            """
                |goal goal1 {
                |    description = "Some description"
                |}
                |""".stripMargin
          )
        )
    }
    test("rejects empty property blocks") {
        assert(
          parsingFails(
            """
                |goal goal1 {}
                |""".stripMargin
          )
        )
    }
    test("rejects empty property values") {
        assert(
          parsingFails(
            """
                |goal goal1 {
                |    informalDef = ""
                |}
                |""".stripMargin
          )
        )
    }
    test("rejects an unclosed property block") {
        assert(
          parsingFails(
            """
                |goal goal1 {
                |    informalDef = "Some definition"
                |""".stripMargin
          )
        )
    }
    test("rejects an unclosed property value") {
        assert(
          parsingFails(
            """
                |goal goal1 {
                |    informalDef = "Some definition
                |}
                |""".stripMargin
          )
        )
    }

// testing inter-element and relationships
    test("reports the source location of a duplicate identifier") {
        val errors =
            validate(
              """
                |goal goal1
                |requirement goal1
                |""".stripMargin
            )

        val error =
            errors
                .find(
                  _.category == ErrorCategory.DuplicateIdentifier
                )
                .get

        assertEquals(
          error.location,
          SourceLocation(3, 1)
        )
    }
    test("reports useful information for an unresolved reference") {
        val errors =
            validate(
              """
            |action action1
            |missingAgent performs action1
            |""".stripMargin
            )

        val error =
            errors
                .find(_.category == ErrorCategory.UnresolvedReference)
                .get

        assertEquals(error.location, SourceLocation(3, 1))
        assert(error.message.contains("missingAgent"))
    }
    test("accepts a domain agent responsible for an assumption") {
        val errors =
            validate(
              """
                |assumption assumption1
                |
                |agent environment {
                |    realm = "domain"
                |}
                |
                |environment is responsible for assumption1
                |""".stripMargin
            )

        assertEquals(
          errors,
          Vector.empty
        )
    }
    test("rejects a software agent responsible for an assumption") {
        val errors =
            validate(
              """
                |assumption assumption1
                |
                |agent a1 {
                |    realm = "software"
                |}
                |
                |a1 is responsible for assumption1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.RealmConstraint
          )
        )
    }
    test("accepts a software agent responsible for a requirement") {
        val errors =
            validate(
              """
                |requirement req1
                |
                |agent environment {
                |    realm = "software"
                |}
                |
                |environment is responsible for req1
                |""".stripMargin
            )

        assertEquals(
          errors,
          Vector.empty
        )
    }
    test("rejects a domain agent responsible for a requirement") {
        val errors =
            validate(
              """
                |requirement req1
                |
                |agent a1 {
                |    realm = "domain"
                |}
                |
                |a1 is responsible for req1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.RealmConstraint
          )
        )
    }
    test("reports conflicting relationships") {
        val errors =
            validate(
              """
            |goal goal1
            |goal goal2
            |
            |goal1 reduces goal2
            |goal1 conflicts goal2
            |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.ConflictingRelationship
          )
        )
    }
    test("reports a cyclic relationship error") {
        val errors =
            validate(
              """
                |goal goal1
                |goal goal2
                |goal goal3
                |
                |goal1 reduces goal2
                |goal2 reduces goal3
                |goal3 reduces goal1
                |""".stripMargin
            )

        assert(
          errors.exists(
            _.category == ErrorCategory.CyclicRelationship
          )
        )
    }
    test("does not report a cycle for an acyclic relationship") {
        val errors =
            validate(
              """
            |goal goal1
            |goal goal2
            |goal goal3
            |
            |goal1 reduces goal2
            |goal2 reduces goal3
            |""".stripMargin
            )

        assert(
          !errors.exists(
            _.category == ErrorCategory.CyclicRelationship
          )
        )
    }
    test("reports multiple validation errors") {

        val errors =
            validate(
              """
                |agent a1
                |goal goal1
                |goal goal1
                |a1 performs missingAction
                |""".stripMargin
            )

        assert(errors.size >= 2)

        assert(
          errors.exists(
            _.category == ErrorCategory.MissingProperty
          )
        )

        assert(
          errors.exists(
            _.category == ErrorCategory.DuplicateIdentifier
          )
        )

        assert(
          errors.exists(
            _.category == ErrorCategory.UnresolvedReference
          )
        )
    }
    test("reports validation errors in source order") {
        val errors =
            validate(
              """
                |agent a1
                |goal goal1
                |goal goal1
                |""".stripMargin
            )

        assertEquals(
          errors,
          errors.sortBy(error => (error.location.line, error.location.column))
        )
    }
