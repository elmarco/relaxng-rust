The design of `relaxng-gen` focuses on converting RelaxNG schemas (XML syntax) into idiomatic Rust code, specifically tailored for the `libvirt` project but generally applicable to similar XML-based modeling tasks.

### 1. Overall Design

The project operates as a compiler pipeline that transforms an input RelaxNG schema into a hierarchy of Rust modules, structs, and enums.

**Key Components:**

*   **Input Processing:**
    *   **Schema Loading:** The tool accepts a RelaxNG schema (typically `.rng`). It uses the `relaxng-model` crate to parse the schema into an intermediate representation (IR).
    *   **Configuration:** It optionally loads a TOML configuration file. This configuration is crucial for customizing the output, as automatic mapping from XML schemas to Rust types often requires human guidance (e.g., naming anonymous types, resolving conflicts).

*   **Generation Logic (`src/rustgen.rs`):**
    *   **Traversal:** The core logic involves traversing the RelaxNG pattern tree using a `Context` and `StateStack`.
    *   **Context:** The `Context` maintains the current state of generation, including a stack of patterns being visited (Elements, Attributes, Choices). It tracks the path (XPath) to the current node to apply configuration rules.
    *   **Unit Generation:** As the traversal exits nodes (`pop_state`), it constructs "Generation Units" (`GenUnit`). These units represent the building blocks of the generated code:
        *   `GenStruct`: Represents an XML element with attributes and child elements.
        *   `GenEnum`: Represents a `<choice>` pattern or a collection of mutually exclusive options.
        *   `GenLib`: Represents a Rust module file, organizing the generated code into a directory structure.

*   **Intermediate Representation (`GenTree`):**
    *   The generated units are stored in a `GenTree`, which maps filesystem paths to `GenUnit`s. This allows the tool to build a complete virtual file system of the generated project before writing it to disk.

*   **Code Output:**
    *   The final step iterates over the `GenTree` and uses the `quote` crate to generate Rust syntax tokens.
    *   It produces standard Rust code with:
        *   `serde`-like serialization/deserialization (via `quick-xml` and custom traits).
        *   Builder patterns for constructing types.
        *   Documentation comments derived from the schema or configuration.

### 2. Handling of Conflicting or Ambiguous Situations

RelaxNG is more flexible than Rust's type system, often allowing patterns that are difficult to map directly to strict structs and enums. The project employs several strategies to handle conflicts and ambiguities:

**A. Unit Reconciliation (`GenUnit::reconcile`)**
When the generator encounters a definition for a type that already exists (e.g., the same element name appearing in two different contexts), it attempts to "reconcile" them.
*   **Struct vs. Struct:** Fields are merged. If a field exists in one but not the other, it becomes `optional` in the unified struct.
*   **Enum vs. Enum:** Variants are merged. Unique variants are added to the combined enum.
*   **Struct vs. Enum:** This is treated as a hard conflict (`Error::Other`), as these types are fundamentally incompatible in Rust. The user must resolve this via configuration (typically by renaming one of the instances).

**B. Field Reconciliation (`GenField::reconcile`)**
When fields within a struct conflict (e.g., merging two structs or encountering repeated patterns):
*   **Type Promotion:**
    *   If two fields have different fixed values (e.g., `Value("a")` vs `Value("b")`), they are promoted to a `Choice` (Enum) containing both values.
    *   If a `Choice` field conflicts with a simple type, the simple type is added as a new variant to the `Choice`.
    *   If `Text` conflicts with a `Parse` type, it generally falls back to `Text`.
*   **Cardinality:**
    *   If a field is present in one path but not another, it is marked `optional`.
    *   If a field appears multiple times or in a loop, it is marked `multiple` (Vec).

**C. Naming Collisions**
*   **Field vs. Attribute:** If an element has both a child element `<foo>` and an attribute `@foo`, the attribute is automatically renamed to `foo_attr` to avoid a struct field name collision.
*   **Reserved Keywords:** All identifiers are passed through sanitization functions (`safe_var_name`, `safe_ty_name`) to avoid clashing with Rust keywords (e.g., `type` -> `type_`, `match` -> `match_`).

### 3. TOML Configuration

The tool uses TOML files to provide fine-grained control over the generation process, primarily targeting nodes via their XPath in the schema.

**File Structure:**
The configuration is typically loaded from files mirroring the schema names (e.g., `libvirt/basictypes.toml`).

**General Configuration:**
```toml
[relaxng-gen]
doc = "..." # Documentation for the root module
```

**Rule Configuration:**
Rules are defined using the `[rule."XPATH"]` syntax, where `XPATH` is the path to the specific node in the RelaxNG schema.

```toml
[rule."/ref[@name='virYesNo']/choice[1]"]
name = "YesNo"                 # Rename the generated Rust type to "YesNo"
doc = "schemas/basictypes.toml:2" # Attach documentation to this type
replace_with_text = true       # Force this node to be treated as a simple String
field_name = "custom_field"    # Rename the field in the parent struct
skip = true                    # Do not generate code for this node
collapse_with_parent = true          # Collapse hierarchy: merge this node into its parent
allow_structure_is_enum = true # Allow a struct to be treated as an enum (e.g., for single-choice wrappers)
```

**Common Use Cases:**
1.  **Naming Anonymous Choices:** `<choice>` patterns often lack names. You can target them via XPath (e.g., `.../choice[1]`) and assign a meaningful `name`.
2.  **Documentation:** External documentation can be injected into the generated code.
3.  **Simplification:** Complex nested structures that are logically simple strings can be flattened using `replace_with_text`.
