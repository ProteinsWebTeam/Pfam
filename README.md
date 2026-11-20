# Pfam

Pfam is a database of protein families and domains represented by multiple sequence alignments and profile hidden Markov models (HMMs). It provides tools and resources for identifying, classifying, and annotating protein sequences.

This repository contains the core Perl codebase supporting Pfam data curation and release procedures.

## Overview

The Pfam codebase provides libraries and scripts used in the management and curation of protein families. It supports a range of activities, including:

- Creating and updating Pfam families.
- Managing family annotations, alignments, and profile HMMs.
- Validating and processing family data.
- Preparing release data.

The codebase is primarily written in Perl and includes database interfaces, reusable modules, command-line utilities, and some tests.

## Repository structure

The repository is organised into four main directories:

| Directory | Description |
|---|---|
| `PfamLib/` | Core Perl libraries implementing Pfam functionality and data processing. |
| `PfamSchemata/` | Database schema definitions and associated Perl classes. |
| `PfamScripts/` | Command-line tools and scripts for curation, analysis, and maintenance. |
| `PfamTests/` | Tests for validating code functionality. |


## Documentation and resources

Further information about Pfam can be found at:

- [Pfam at InterPro](https://www.ebi.ac.uk/interpro/entry/pfam/)
- [Pfam documentation](https://pfam-docs.readthedocs.io/)
- [InterPro](https://www.ebi.ac.uk/interpro/)
- [HMMER](http://hmmer.org/)
