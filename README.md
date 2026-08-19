# KAOS DSL
This project implements the KAOS UML profile defined by Heaven and Finkelstein in 2004. It is extended by the element 'Obstacle'.
### Getting Started

#### Prerequisites

- Java JDK 17

- sbt (version 1.12.14 was used during development)

- Graphviz (optional, required for visualization)

Scala 3.8.4 is used by this project.


In order to run the code, the command `sbt run` can be used in the kaos-dsl directory.
To run the tests, `sbt test` can be used.

#### Defining a model

A KAOS model can be defined in model.kaos. It is the default file for the input. Custom file sources can be declared by running `sbt "run custom/path/model.kaos"`.