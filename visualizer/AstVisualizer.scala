package kaos.visualizer

import kaos.model.*

import java.nio.file.{Files, Path, Paths}

object AstVisualizer:

    def createVisualizations(
        model: KaosModel,
        outputPath: Path
    ): Unit =
        val parent =
            Option(outputPath.getParent).getOrElse(Paths.get("."))

        val fileName =
            outputPath.getFileName.toString

        val baseName =
            fileName.lastIndexOf('.') match
                case index if index > 0 =>
                    fileName.substring(0, index)
                case _ =>
                    fileName

        writeAndRender(
          modelToDot(model),
          parent.resolve(s"$baseName.dot"),
          outputPath
        )

        writeAndRender(
          modelToUmlDot(model),
          parent.resolve(s"${baseName}_uml.dot"),
          parent.resolve(s"${baseName}_uml.png")
        )

    private def writeAndRender(
        dot: String,
        dotPath: Path,
        imagePath: Path
    ): Unit =
        Files.writeString(dotPath, dot)
        createImage(dotPath, imagePath)

    private def modelToDot(
        model: KaosModel
    ): String =
        val builder =
            new StringBuilder

        builder.append(
          """
              |digraph KaosAST {
              |    rankdir=TB;
              |    node [shape=box];
              |
              |    model [label="KaosModel"];
              |
              |""".stripMargin
        )

        model.elements.zipWithIndex.foreach { case (element, elementIndex) =>
            val elementNode =
                s"element$elementIndex"

            builder.append(
              s"""    $elementNode [
                       |        label="ElementNode\\nelementType = ${escape(
                  element.elementType.toString
                )}\\nid = ${escape(
                  element.id
                )}\\nlocation = ${element.location.line}:${element.location.column}"
                       |    ];
                       |
                       |    model -> $elementNode [
                       |        label="elements"
                       |    ];
                       |
                       |""".stripMargin
            )

            element.properties.zipWithIndex.foreach {
                case (property, propertyIndex) =>
                    val propertyNode =
                        s"property${elementIndex}_$propertyIndex"

                    builder.append(
                      s"""    $propertyNode [
                               |        label="PropertyAssignment\\nname = ${escape(
                          property.name.toString
                        )}\\nvalue = ${escape(
                          property.value
                        )}\\nlocation = ${property.location.line}:${property.location.column}"
                               |    ];
                               |
                               |    $elementNode -> $propertyNode [
                               |        label="properties"
                               |    ];
                               |
                               |""".stripMargin
                    )
            }
        }

        model.relationships.zipWithIndex.foreach {
            case (relationship, relationshipIndex) =>
                val relationshipNode =
                    s"relationship$relationshipIndex"

                builder.append(
                  s"""    $relationshipNode [
                       |        label="RelationshipNode\\nrelationshipType = ${escape(
                      relationship.relationshipType.toString
                    )}\\nsourceId = ${escape(
                      relationship.sourceId
                    )}\\ntargetId = ${escape(
                      relationship.targetId
                    )}\\nlocation = ${relationship.location.line}:${relationship.location.column}"
                       |    ];
                       |
                       |    model -> $relationshipNode [
                       |        label="relationships"
                       |    ];
                       |
                       |""".stripMargin
                )
        }

        builder.append("}\n")
        builder.toString

    private def modelToUmlDot(
        model: KaosModel
    ): String =
        val builder =
            new StringBuilder

        builder.append(
          """
              |digraph KaosUmlProfile {
              |    rankdir=TB;
              |
              |    graph [
              |        splines=true,
              |        overlap=false,
              |        nodesep=0.7,
              |        ranksep=1.0
              |    ];
              |
              |    node [shape=plain];
              |    edge [fontsize=10];
              |
              |""".stripMargin
        )

        model.elements.foreach { element =>
            builder.append(umlElementNode(element))
        }

        model.relationships.foreach { relationship =>
            builder.append(umlRelationshipEdge(relationship))
        }

        builder.append("}\n")
        builder.toString

    private def umlElementNode(
        element: ElementNode
    ): String =
        val stereotypeRow =
            umlStereotype(element.elementType) match
                case Some(stereotype) =>
                    s"""
                       |<TR>
                       |    <TD>&lt;&lt;${escapeHtml(stereotype)}&gt;&gt;</TD>
                       |</TR>
                       |""".stripMargin
                case None =>
                    ""

        val tagRows =
            element.properties
                .map { property =>
                    s"${property.name.syntax} = ${property.value}"
                }
                .map { tag =>
                    s"""
                       |<TR>
                       |    <TD ALIGN="LEFT">{${escapeHtml(tag)}}</TD>
                       |</TR>
                       |""".stripMargin
                }
                .mkString("\n")

        val label =
            s"""
               |<
               |<TABLE BORDER="1"
               |       CELLBORDER="0"
               |       CELLSPACING="0"
               |       CELLPADDING="5">
               |    $stereotypeRow
               |    <TR>
               |        <TD><B>${escapeHtml(element.id)}</B></TD>
               |    </TR>
               |    $tagRows
               |</TABLE>
               |>
               |""".stripMargin

        s"""    "${escape(element.id)}" [
           |        label=$label
           |    ];
           |
           |""".stripMargin

    private def umlRelationshipEdge(
        relationship: RelationshipNode
    ): String =
        val stereotype =
            relationship.relationshipType.toString.toLowerCase

        val (source, target, style) =
            relationship.relationshipType match
                case RelationshipType.Reduces =>
                    (
                      relationship.targetId,
                      relationship.sourceId,
                      "style=dashed,"
                    )
                case _ =>
                    (
                      relationship.sourceId,
                      relationship.targetId,
                      ""
                    )

        s"""    "${escape(source)}" ->
           |    "${escape(target)}" [
           |        label="<<$stereotype>>",
           |        $style
           |        dir=none
           |    ];
           |
           |""".stripMargin

    private def umlStereotype(
        elementType: ElementType
    ): Option[String] =
        elementType match
            case ElementType.Object =>
                Some("kobject")
            case ElementType.Entity =>
                Some("kentity")
            case ElementType.Event =>
                Some("kevent")
            case ElementType.Action =>
                Some("kaction")
            case ElementType.Agent =>
                Some("kagent")
            case ElementType.Obstacle =>
                None
            case other =>
                Some(other.toString.toLowerCase)

    private def createImage(
        dotPath: Path,
        imagePath: Path
    ): Unit =
        val process =
            new ProcessBuilder(
              "dot",
              "-Tpng",
              dotPath.toString,
              "-o",
              imagePath.toString
            )
                .inheritIO()
                .start()

        val exitCode =
            process.waitFor()

        if exitCode == 0 then println(s"Visualization created: $imagePath")
        else println(s"Graphviz failed with exit code $exitCode")

    private def escape(
        text: String
    ): String =
        text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
            .replace("\r", "")

    private def escapeHtml(
        text: String
    ): String =
        text
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
