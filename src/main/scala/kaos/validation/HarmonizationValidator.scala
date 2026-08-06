package kaos.validation

import kaos.feedback.*
import kaos.model.*

import scala.collection.mutable

object HarmonizationValidator:

    def validate(
        model: KaosModel
    ): Vector[ValidationError] =
        validateDuplicateRelationships(model) ++
            validateConflictingRelationships(model) ++
            validateCyclicRelationships(model)

// Makes sure the relationship hasnt been declared before
    private def validateDuplicateRelationships(
        model: KaosModel
    ): Vector[ValidationError] =
        model.relationships
            .groupBy { relationship =>
                (
                  relationship.relationshipType,
                  relationship.sourceId,
                  relationship.targetId
                )
            }
            .values
            .flatMap { relationships =>
                val sorted =
                    sortByLocation(relationships)

                val original =
                    sorted.head

                sorted.drop(1).map { duplicate =>
                    ValidationError(
                      category = ErrorCategory.DuplicateRelationship,
                      location = duplicate.location,
                      message =
                          s"Relationship '${renderRelationship(duplicate)}' was already declared at line ${original.location.line}."
                    )
                }
            }
            .toVector
// Make sure the Source and Target dont have multiple conflicting relationships declared. (reduces & conflicts)
    private def validateConflictingRelationships(
        model: KaosModel
    ): Vector[ValidationError] =
        model.relationships
            .groupBy { relationship =>
                (
                  relationship.sourceId,
                  relationship.targetId
                )
            }
            .values
            .flatMap { relationships =>
                val sorted =
                    sortByLocation(relationships)

                val reduces =
                    sorted.find {
                        _.relationshipType == RelationshipType.Reduces
                    }

                val conflicts =
                    sorted.find {
                        _.relationshipType == RelationshipType.Conflicts
                    }

                (reduces, conflicts) match
                    case (
                          Some(reducesRelationship),
                          Some(conflictsRelationship)
                        ) =>
                        val ordered =
                            sortByLocation(
                              Vector(
                                reducesRelationship,
                                conflictsRelationship
                              )
                            )

                        val earlier =
                            ordered.head

                        val later =
                            ordered.last

                        Vector(
                          ValidationError(
                            category = ErrorCategory.ConflictingRelationship,
                            location = later.location,
                            message =
                                s"Relationship '${renderRelationship(later)}' conflicts with relationship '${renderRelationship(earlier)}' declared at line ${earlier.location.line}."
                          )
                        )

                    case _ =>
                        Vector.empty
            }
            .toVector
// Filters all relationships and checks reduces relationships on cycles
    private def validateCyclicRelationships(
        model: KaosModel
    ): Vector[ValidationError] =
        val reducesRelationships =
            model.relationships.filter {
                _.relationshipType == RelationshipType.Reduces
            }

        validateCyclesForRelationshipType(reducesRelationships)
// Builds directional graph and checks if there would be circles
    private def validateCyclesForRelationshipType(
        relationships: Seq[RelationshipNode]
    ): Vector[ValidationError] =
        val adjacency =
            mutable.Map.empty[
              String,
              Vector[RelationshipNode]
            ]

        val errors =
            Vector.newBuilder[ValidationError]

        sortByLocation(relationships).foreach { relationship =>
            findPath(
              startId = relationship.targetId,
              targetId = relationship.sourceId,
              adjacency = adjacency
            ).foreach { path =>
                errors += ValidationError(
                  category = ErrorCategory.CyclicRelationship,
                  location = relationship.location,
                  message = s"Relationship '${renderRelationship(relationship)}' " +
                      s"creates a cycle: ${renderCycle(path :+ relationship)}."
                )
            }

            adjacency.update(
              relationship.sourceId,
              adjacency.getOrElse(
                relationship.sourceId,
                Vector.empty
              ) :+ relationship
            )
        }

        errors.result()
// Path finding in the graph created in validateCyclesForRelationshipType
    private def findPath(
        startId: String,
        targetId: String,
        adjacency: scala.collection.Map[
          String,
          Vector[RelationshipNode]
        ]
    ): Option[Vector[RelationshipNode]] =

        def visit(
            currentId: String,
            visited: Set[String]
        ): Option[Vector[RelationshipNode]] =
            if currentId == targetId then Some(Vector.empty)

            else if visited.contains(currentId) then None

            else
                adjacency
                    .getOrElse(
                      currentId,
                      Vector.empty
                    )
                    .iterator
                    .map { relationship =>
                        visit(
                          currentId = relationship.targetId,
                          visited = visited + currentId
                        ).map { remainingPath =>
                            relationship +: remainingPath
                        }
                    }
                    .collectFirst { case Some(path) =>
                        path
                    }

        visit(
          currentId = startId,
          visited = Set.empty
        )
// sorting relationships by location
    private def sortByLocation(
        relationships: Iterable[RelationshipNode]
    ): Vector[RelationshipNode] =
        relationships.toVector.sortBy { relationship =>
            (
              relationship.location.line,
              relationship.location.column
            )
        }
// creates readable text for the error message
    private def renderRelationship(
        relationship: RelationshipNode
    ): String =
        s"${relationship.sourceId} ${relationship.relationshipType.phrase} ${relationship.targetId}"
// creates readable text for the error message
    private def renderCycle(
        relationships: Vector[RelationshipNode]
    ): String =
        val firstRelationship =
            relationships.head

        val remainingParts =
            relationships.map { relationship =>
                s"${relationship.relationshipType.phrase} " +
                    s"${relationship.targetId}"
            }

        (
          firstRelationship.sourceId +: remainingParts
        ).mkString(" ")
