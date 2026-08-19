import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import kaos.model.*
import kaos.parser.KaosParser
import kaos.validation.Validator

import org.parboiled2.ParseError

import scala.util.{Failure, Success}

@main
def runKaosDsl(arguments: String*): Unit =
    val filePath =
        arguments.headOption.getOrElse("examples/model.kaos")

    val path = Path.of(filePath)

    val visualizationPath =
        Option(path.getParent)
            .getOrElse(Path.of("."))
            .resolve("ast.svg")

    if !Files.exists(path) then
        println(s"Input file '$filePath' does not exist.")
    else
        val input =
            Files.readString(path, StandardCharsets.UTF_8)

        val parser =
            new KaosParser(input)

        parser.Input.run() match
            case Success(model) =>
                visualizeIfPossible(model, visualizationPath)

                val errors =
                    Validator.validate(model)

                if errors.isEmpty then printSuccessfulModel(model)
                else
                    println(
                      s"Validation failed with ${errors.size} error(s):"
                    )
                    errors.foreach(error => println(s"- $error"))

            case Failure(error: ParseError) =>
                println("Syntax Error:")
                println(parser.formatError(error))

            case Failure(error) =>
                println(
                  s"Unexpected parser error: ${error.getMessage}"
                )

private def printSuccessfulModel(
    model: KaosModel
): Unit =
    println("Parsing and validation successful.")
    println()

    println(s"Elements (${model.elements.size}):")

    model.elements.foreach { element =>
        println(
          s"- ${element.elementType} ${element.id}"
        )

        element.properties.foreach { property =>
            println(s"""    ${property.name.syntax} = "${property.value}"""")
        }
    }

    println()
    println(s"Relationships (${model.relationships.size}):")

    model.relationships.foreach { relationship =>
        println(
          s"- ${relationship.sourceId} " +
              s"${relationship.relationshipType.phrase} " +
              s"${relationship.targetId}"
        )
    }
    println()

private def visualizeIfPossible(
    model: KaosModel,
    outputPath: Path
): Unit =
    try
        val classs =
            Class.forName("kaos.visualizer.AstVisualizer$")

        val instance =
            classs.getField("MODULE$").get(null)

        classs
            .getMethod(
              "createVisualizations",
              classOf[KaosModel],
              classOf[Path]
            )
            .invoke(instance, model, outputPath)

    catch
        case _: ClassNotFoundException =>
            ()
