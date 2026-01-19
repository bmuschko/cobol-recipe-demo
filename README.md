# COBOL Recipe Demo

This project demonstrates the use of [OpenRewrite](https://docs.openrewrite.org/) COBOL recipes with the [Moderne CLI](https://docs.moderne.io/user-documentation/moderne-cli/getting-started/cli-intro/).

## Project Structure

```
src/
└── main/
    ├── cobol/
    │   ├── CUSTMGMT.cbl    # Customer management program with debugging mode
    │   ├── CUSTSQL.cbl     # SQL processing program with EXEC SQL statements
    │   ├── CUSTBATCH.cbl   # Batch processing program with COPY REPLACING
    │   └── CUSTCICS.cbl    # CICS transaction program with EXEC CICS statements
    └── copylib/
        ├── CUSTOMER.cpy    # Customer record copybook
        └── SQLCA.cpy       # SQL communication area copybook
```

## Prerequisites

- [Moderne CLI](https://docs.moderne.io/user-documentation/moderne-cli/getting-started/cli-intro/) version 3.55.1 or higher
- Java 8 or higher

## Configuration

The project uses a `.moderne/moderne.yml` file to configure the COBOL build step:

```yaml
specs: specs.moderne.ai/v1/cli
build:
  steps:
    - type: cobol
      cobolSourceDirectories:
        - src/main/cobol
      copybookDirectories:
        - src/main/copylib
```

## Setup

### 1. Install the recipe JAR

```bash
mod config recipes jar install org.openrewrite:rewrite-cobol:2.13.0
```

### 2. Build the LST (Lossless Semantic Tree)

```bash
mod build . --no-download
```

## Available Recipes

The `rewrite-cobol` module (version 2.13.0) provides the following recipes:

| Recipe | Description |
|--------|-------------|
| `org.openrewrite.cobol.cleanup.RemoveWithDebuggingMode` | Removes `WITH DEBUGGING MODE` from SOURCE-COMPUTER paragraphs |
| `org.openrewrite.cobol.search.FindCopybook` | Finds all COPY statements and EXEC SQL INCLUDE statements |
| `org.openrewrite.cobol.search.FindIndicators` | Finds lines with specific indicator characters (e.g., 'D' for debug lines) |
| `org.openrewrite.cobol.search.FindRelationships` | Builds a list of relationships between COBOL resources (programs, copybooks, subroutines) |
| `org.openrewrite.cobol.search.FindWord` | Searches for COBOL words based on a search term |
| `org.openrewrite.cobol.search.FindReference` | Finds matching identifiers in COBOL, copybooks, and JCL |

## Tested Use Cases

| Recipe | Command | Result |
|--------|---------|--------|
| `RemoveWithDebuggingMode` | `mod run . --recipe=org.openrewrite.cobol.cleanup.RemoveWithDebuggingMode` | Produced fix.patch - Removed `WITH DEBUGGING MODE` from 2 files |
| `FindIndicators` | `mod run . --recipe=org.openrewrite.cobol.search.FindIndicators -P indicator=D` | Produced search.patch - Found 23 debug lines across 2 files |
| `FindRelationships` | `mod run . --recipe=org.openrewrite.cobol.search.FindRelationships` | Produced search.patch - Found COBOL relationships |
| `FindCopybook` | `mod run . --recipe=org.openrewrite.cobol.search.FindCopybook` | Completed (produced data tables) |
| `FindWord` | `mod run . --recipe=org.openrewrite.cobol.search.FindWord -P searchTerm=CUST-ID -P exactMatch=true` | Produced search.patch - Found 4 occurrences of CUST-ID |
| `FindReference` | `mod run . --recipe=org.openrewrite.cobol.search.FindReference -P searchTerm=CUSTVAL -P exactMatch=true` | Completed (searched for subroutine reference) |
| `FindRelationships` (CICS) | `mod run . --recipe=org.openrewrite.cobol.search.FindRelationships` | Produced search.patch - Found CICS program relationships including CUSTCICS COPY dependency |

## Example Results

### RemoveWithDebuggingMode

This recipe removes the `WITH DEBUGGING MODE` clause from SOURCE-COMPUTER paragraphs:

```diff
-       SOURCE-COMPUTER. IBM-390 WITH DEBUGGING MODE.
+       SOURCE-COMPUTER. IBM-390                    .
```

### FindIndicators (D)

This recipe finds all debug lines (lines with 'D' in column 7) and marks them:

```diff
-      D    DISPLAY 'DEBUG: INITIALIZING PROGRAM'.
+      ~~>D    DISPLAY 'DEBUG: INITIALIZING PROGRAM'.
```

### FindWord (CUST-ID)

This recipe finds all occurrences of a specific word and marks them:

```diff
-               RECORD KEY IS CUST-ID
+               RECORD KEY IS ~~>CUST-ID
```

### FindRelationships (CICS Program)

The `CUSTCICS.cbl` program demonstrates CICS support. Running the FindRelationships recipe identifies the CICS program and its copybook dependency:

**Command:**
```bash
mod run . --recipe=org.openrewrite.cobol.search.FindRelationships
```

**Search patch output for CUSTCICS.cbl:**
```diff
-       COPY CUSTOMER.
+       COPY ~~>CUSTOMER.
```

**CobolRelationships data table output:**
```bash
mod study . --last-recipe-run --data-table CobolRelationships
```

| dependent | dependentType | action | dependency | dependencyType |
|-----------|---------------|--------|------------|----------------|
| CUSTCICS  | COBOL         | COPY   | CUSTOMER   | COPYBOOK       |

**Note:** The CICS program is parsed successfully and standard COBOL relationships (COPY statements) are identified. EXEC CICS statements are recognized by the parser but their internal commands are treated as opaque blocks rather than being fully analyzed for relationships.

## Applying Changes

To apply the changes from a recipe run:

```bash
# Apply the last recipe run
mod git apply . --last-recipe-run

# Or apply a specific recipe run
mod git apply . --recipe-run <recipe-run-id>
```

## Viewing Data Tables

Some recipes produce data tables with additional information:

```bash
mod study . --last-recipe-run --data-table RecipeRunStats
mod study . --last-recipe-run --data-table CobolRelationships
mod study . --last-recipe-run --data-table SearchResults
```

## Resources

- [OpenRewrite Documentation](https://docs.openrewrite.org/)
- [Moderne CLI Documentation](https://docs.moderne.io/user-documentation/moderne-cli/getting-started/cli-intro/)
- [rewrite-cobol on Maven Central](https://central.sonatype.com/artifact/org.openrewrite/rewrite-cobol)
