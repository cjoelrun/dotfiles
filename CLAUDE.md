# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository containing configuration files for various development tools and environments. The repository is primarily focused on macOS development with extensive Emacs configuration and support for multiple programming languages.

## Key Commands

### Initial Setup
```bash
# Install TypeScript language server (minimal setup script available)
./init-osx.sh
```

### Git Operations
Common git aliases configured in `.gitconfig`:
- `git lol` - Pretty formatted log with graph
- `git cb` - Create and switch to new branch
- `git sb` - Switch branch
- `git db` - Delete branch
- `git mru` - Show most recently used branches

### Development Tools
The repository expects these tools to be available:
- **Node.js**: Managed via NVM
- **Ruby**: Managed via RVM
- **Java/Kotlin/Gradle**: Managed via SDKMAN
- **Rust**: Via Cargo
- **Go**: Standard Go installation
- **Python**: System Python with virtualenvwrapper

## Architecture and Structure

### Directory Organization
```
/
├── config/           # Application-specific configurations
│   ├── gh/          # GitHub CLI config
│   ├── karabiner/   # Keyboard remapping (complex Emacs keybindings)
│   ├── iterm2/      # Terminal emulator settings
│   └── stripe/      # Stripe CLI config
├── emacs.d/         # Extensive Emacs configuration
│   ├── external/    # External package configurations
│   └── straight/    # Straight.el package management
├── xmonad/          # Window manager configuration
└── [shell configs]  # bashrc, zshrc, profile, bash_profile
```

### Shell Configuration Strategy
- **Common configuration**: Loaded via `profile`
- **Bash-specific**: `bashrc` and `bash_profile`
- **Zsh-specific**: `zshrc`
- **Secrets**: All shells source `~/.secrets` for sensitive data
- **Editor preference**: Emacs for Bash, VS Code for Zsh

### Emacs Configuration
The Emacs setup (`emacs.d/`) is sophisticated with:
- **Package management**: Both ELPA and Straight.el
- **LSP support**: Configured for Python, Rust, TypeScript, Go, and more
- **Key packages**: Magit, Projectile, Ivy/Counsel, Company, Flycheck
- **Copilot integration**: Code exists but is commented out in `external/external.el`

### Security Considerations
- Sensitive data should be stored in `~/.secrets` (sourced by all shells)
- The repository uses `.aider` as a global git exclude file
- GitHub Copilot tokens are currently stored in `config/github-copilot/apps.json` (consider moving to `.secrets`)

## Important Notes

1. **Multi-environment support**: Configurations support both macOS and Linux (different SDKMAN paths)
2. **No comprehensive installation script**: Only minimal `init-osx.sh` exists
3. **Editor context**: Heavy Emacs user with VS Code as secondary editor
4. **Path management**: Extensive PATH modifications across shell configs - be careful not to duplicate
5. **Git workflow**: Uses custom aliases and excludes `.aider` files globally

## Common Development Scenarios

When modifying shell configurations:
1. Changes to `bashrc` affect Bash shells only
2. Changes to `zshrc` affect Zsh shells only
3. Common changes should go in `profile`
4. Remember to source `~/.secrets` in any new shell configs

When working with Emacs configuration:
1. Package installations use Straight.el (see `emacs.d/init.el`)
2. External package configs go in `emacs.d/external/external.el`
3. LSP configurations are already set up for major languages

When adding new application configs:
1. Place them under `config/[app-name]/`
2. Follow the existing pattern of subdirectory organization
3. Consider security implications for any tokens or credentials