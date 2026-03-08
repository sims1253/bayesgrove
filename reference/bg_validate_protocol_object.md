# Validate a protocol object against its schema

Performs structural validation of a protocol object against expected
field names, scalar types, nested arrays, object maps, and local schema
references. This remains lighter than a full JSON Schema validator, but
it is strong enough to catch drift in the checked-in protocol surfaces.

## Usage

``` r
bg_validate_protocol_object(object, schema_name)
```

## Arguments

- object:

  The R object to validate

- schema_name:

  Name of the schema to validate against

## Value

TRUE if valid, otherwise raises an error.
