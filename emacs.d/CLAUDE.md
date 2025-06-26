# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal Emacs configuration repository with a modular structure. The configuration is split between platform-specific settings and external package configurations.

## Architecture

- `init.el` - Main configuration file with core Emacs settings and platform detection
- `external/external.el` - External package configurations using use-package
- `lisp/osx.el` - macOS-specific settings (fonts, clipboard integration)
- `lisp/linux.el` - Linux-specific settings
- `elpa/` - Package.el managed packages
- `straight/` - straight.el managed packages (modern package manager)

The configuration uses both package.el (ELPA/MELPA) and straight.el for package management. Platform detection automatically loads appropriate OS-specific configurations.

## Key Features

- LSP integration for multiple languages (Python, Rust, Go, Swift, JavaScript/TypeScript)
- Git integration via Magit and Forge 
- Project management with Projectile
- Note-taking with Deft (synced to iCloud Obsidian)
- AI integration (GPTel, Aider)
- Modern completion with Ivy/Counsel
- Debugging support via DAP mode

## Language Support

- **Python**: LSP via lsp-pyright, DAP debugging configured
- **Rust**: LSP with format-on-save, DAP debugging setup (commented out)
- **Go**: LSP integration
- **Swift**: LSP via sourcekit-lsp, treesitter support
- **JavaScript/TypeScript**: Web-mode with LSP, 2-space indentation
- **Dockerfile**: LSP support
- **Markdown**: Pandoc integration
- **LaTeX**: AucTeX configuration

## Development Workflow

The configuration automatically:
- Byte-compiles init.el on save
- Saves all buffers on focus loss
- Cleans whitespace before save
- Auto-reverts buffers when files change on disk

## Package Management

Uses dual package managers:
- `package.el` for standard packages (configured in external.el)
- `straight.el` for GitHub packages and modern dependency management

Auto-updates packages via auto-package-update.