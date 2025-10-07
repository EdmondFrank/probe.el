# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2025-10-07

### Added
- Enhanced syntax highlighting with better error handling
- Comprehensive test suite with test functions for syntax highlighting and search highlighting
- File collapse/expand functionality in search results
- Improved search term highlighting using word boundary matching

### Changed
- Simplified syntax highlighting implementation for better performance
- Enhanced error handling throughout the codebase
- Improved search results UI with better visual organization

### Fixed
- Better handling of syntax highlighting failures
- Improved word boundary matching in search highlighting

## [0.1.0] - 2024-10-07

### Added
- Initial release of probe.el package
- AI-friendly semantic code search integration
- AST-based query support for structural code search
- Project-aware searching with automatic project root detection
- Configurable reranking algorithms (BM25, hybrid, TF-IDF, and various MS MARCO models)
- Interactive search results with navigation key bindings
- Test file inclusion/exclusion toggle
- Enhanced UI with syntax highlighting and file organization
- Robust error handling throughout the codebase
- Multiple search modes: text search and AST query
- Dedicated commands for method, class, and function searches
- Customizable display buffer functions
- Face customization support for search results appearance

[Unreleased]: https://github.com/edmondfrank/probe.el/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/edmondfrank/probe.el/releases/tag/v0.2.0
[0.1.0]: https://github.com/edmondfrank/probe.el/releases/tag/v0.1.0
