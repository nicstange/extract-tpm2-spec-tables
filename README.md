# *`extract-tpm2-spec-tables`*
Extract Command/Response, Type definition and Algorithm tables from
the TCG TPM2 Library specification PDF documents.

## Background
The [TPM 2.0
Library](https://trustedcomputinggroup.org/resource/tpm-library-specification/)
PDF documents specify the TPM2 interface protocol in terms of
"Command"/"Response" tables and supporting "Structures" tables,
strictly following a certain format each. These table format
conventions are applied throughout to faciliate automatic processing,
interface code generation in particular, c.f. [Trusted Platform Module Library, Part 2: Structures, section 4:
Notation](https://trustedcomputinggroup.org/wp-content/uploads/TCG_TPM2_r1p59_Part2_Structures_pub.pdf):

> The information in this document is formatted so that it may be converted to standard computer-language
> formats by an automated process. The purpose of this automated process is to minimize the transcription
> errors that often occur during the conversion process.

However, to my knowledge, the Command/Response and Structures tables are only distributed inline as part of the TGC TPM2
Library PDF documents and not made available by the TCG in a readibly machine-processable format.

From searching the web, I found two instances of somewhat more accessible versions of the TCG TPM2 Library
specification's command/response and type definition tables:
- The [Microsoft TSS.MSR Github
  project](https://github.com/microsoft/TSS.MSR/tree/bda0a44643064ebc4984f6c5568563a0c5eef23f/TssCodeGen/TpmSpec)
  distributes the tables as extracted into one large XML file. The fact that the TCG TPM2 Library specification is
  distributed alongside in Microsoft Word `*.docx` format suggests that the letter is the result of some table
  extraction from `*.docx`. However, it's unclear[^1] how the `*.docx` versions of the TCG TPM2 Library have been
  obtained in the first place.
- The TPM2 code distributed with the [Android sources](https://android.googlesource.com/platform/external/tpm2/) comes
  with a copy of the TCG TPM2 Library tables in HTML format for further processing. Supposedly [these have been
  extracted from the
  PDFs](https://android.googlesource.com/platform/external/tpm2/+/6c62c47e1e3b90acb6525a934941c58a8a5c9faf) manually
  using some table extraction feature of the "Adobe Acrobat Editor".

However, neither of these approaches is easily reproducible and in particular, they don't allow for (independent)
adoptions of potential future standard changes, like e.g to the TCG Algorithm Registry.

*`extract-tpm2-spec-tables`* is supposed to fill the gap: it's purpose
is to extract all relevant tables from the TCG TPM2 Library
specification in PDF format and output them in a certain ASCII format for
further processing by e.g. interface code generators.

[^1]: The officially distributed PDF documents have got a PDF metadata "Creator" annotation of "Microsoft® Word for
      Office 365", suggesting that they have originally been authored in in Microsoft Word. However, it seems like the
      documents have never been published officially in Microsoft Word format.

## Compilation
A plain
```
cargo build
```
from the top-level source directory will do. After successful compilation, you will find the resulting binaries
at `./target/debug/extract-tpm2-spec-tables`.

## Usage

The usage is straight-forward: you pass *`extract-tpm2-spec-tables`* one PDF file from the TCG TPM2 Library
specification at a time and it will output all extracted tables in text format to its standard-out:
```
./target/release/extract-tpm2-spec-tables [-s <SOURCE-DOCUMENT-NAME>] <INPUT-PDF-FILE>
```
At your option, provide a `SOURCE-DOCUMENT-NAME` as a reference, which will be replicated at each emitted table's header
information.

### Examples
```
extract-tpm2-spec-tables  -s 'TCG TPM2 Library (rev 01.59), Part 2' TCG_TPM2_r1p59_Part2_Structures_pub.pdf > tpm2_structures.csv

```

```
extract-tpm2-spec-tables  -s 'TCG TPM2 Library (rev 01.59), Part 3' TCG_TPM2_r1p59_Part3_Commands_pub.pdf > tpm2_commands.csv

```

```
extract-tpm2-spec-tables  -s 'TCG Algorithm Registry (rev 01.32)' TCG-_Algorithm_Registry_r1p32_pub.pdf > tpm2_algorithms.csv
```
