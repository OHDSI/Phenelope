# Phenelope

# Introduction

The goal of Phenelope is to provide a tool for OMOP users to create a concept set. Requires access to an OMOP CDM and to a Large Language Model (LLM) with API access.

# Process Steps

1)  Determine the health condition of interest
2)  Find an approriate conceptId that is a good starting point for creating a concept set
3)  Set up parameters to run the function createConceptSet()
4)  Run createConceptSet()

# Technology

Phenelope is an R package.

# System Requirements

Requires R (version 4.4.0 or higher).

# Installation

1)  See the instructions at [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2)  Install Phenelope using:

    ```         
    remotes::install_github("OHDSI/Phenelope")
    ```

# User Documentation

1)  Documentation can be found on the [package website](https://ohdsi.github.io/Phenelope).
2)  PDF versions of the documentation are also available:
    -   Package manual: [CohortDiagnostics manual](https://raw.githubusercontent.com/OHDSI/CohortDiagnostics/main/extras/CohortDiagnostics.pdf)

    -   Vignette: [Creating LLM Concept Sets](https://raw.githubusercontent.com/OHDSI/Phenelope/vignettes/CreatingLLMConceptSets.rmd)

# Support

-   Developer questions/comments/feedback: [OHDSI Forum](http://forums.ohdsi.org/c/developers)

-   We use the [GitHub issue tracker](https://github.com/OHDSI/CohortDiagnostics/issues) for all bugs/issues/enhancements

# Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

# License

Phenelope is licensed under Apache License 2.0

# Development

Phenelope is being developed in R Studio.

### Development status

Beta

# Acknowledgements

-   The package is maintained by Joel Swerdel and has been developed with major contributions from Martijn Schuemie and Anna Ostropolets.
