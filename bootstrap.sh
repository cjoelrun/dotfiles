#!/usr/bin/env bash

# macOS Setup Bootstrap Script
# This script sets up a new Mac with your dotfiles (managed by 'dotfiles' command) and development environment
# Run with: ./bootstrap.sh

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Install Xcode Command Line Tools
install_xcode_clt() {
    log_info "Checking for Xcode Command Line Tools..."
    if ! command_exists xcode-select; then
        log_info "Installing Xcode Command Line Tools..."
        xcode-select --install
        log_warning "Please complete the Xcode Command Line Tools installation and press Enter to continue..."
        read -r
    else
        log_success "Xcode Command Line Tools already installed"
    fi
}

# Install Homebrew
install_homebrew() {
    log_info "Checking for Homebrew..."
    if ! command_exists brew; then
        log_info "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

        # Add Homebrew to PATH for Apple Silicon Macs
        if [[ $(uname -m) == 'arm64' ]]; then
            echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi

        log_success "Homebrew installed successfully"
    else
        log_success "Homebrew already installed"
    fi
}

# Setup dotfiles symlinks
setup_dotfiles() {
    log_info "Setting up dotfiles symlinks..."

    # Check if dotfiles command exists
    if ! command_exists dotfiles; then
        log_error "dotfiles command not found! Please install it first."
        log_info "You can install it with: brew install dotfiles"
        return 1
    fi

    # Sync all symlinks
    dotfiles --sync --force

    # Check for any issues
    if dotfiles --check | grep -q "missing\|broken"; then
        log_warning "Some dotfiles may have issues. Run 'dotfiles --check' to see details."
    else
        log_success "All dotfiles symlinks created successfully"
    fi
}

# Install packages from Brewfile
install_packages() {
    log_info "Installing packages from Brewfile..."

    if [[ -f "Brewfile" ]]; then
        brew bundle --file=Brewfile
        log_success "Packages installed successfully"
    else
        log_error "Brewfile not found!"
        exit 1
    fi
}

# Apply macOS system preferences
apply_macos_defaults() {
    log_info "Applying macOS system preferences..."

    if [[ -f ".macos" ]]; then
        chmod +x .macos
        ./.macos
        log_success "macOS preferences applied"
    else
        log_warning ".macos script not found, skipping system preferences"
    fi
}

# Setup Karabiner-Elements
setup_karabiner() {
    log_info "Setting up Karabiner-Elements..."

    if [[ -d "~/.config/karabiner" ]]; then
        # Copy complex modifications if they exist
        if [[ -d "~/.config/karabiner/assets/complex_modifications" ]]; then
            cp -r ~/.config/karabiner/assets/complex_modifications/* ~/.config/karabiner/assets/complex_modifications/ 2>/dev/null || true
        fi

        # Restart Karabiner service
        launchctl kickstart -k gui/$(id -u)/org.pqrs.karabiner.karabiner_console_user_server 2>/dev/null || true

        log_success "Karabiner-Elements configured"
    else
        log_warning "Karabiner config not found, skipping"
    fi
}

# Setup yabai and skhd
setup_window_manager() {
    log_info "Setting up yabai and skhd..."

    # Start yabai service
    if command_exists yabai; then
        brew services start yabai
        log_success "yabai service started"
    fi

    # Start skhd service
    if command_exists skhd; then
        brew services start skhd
        log_success "skhd service started"
    fi
}

# Setup development environment
setup_dev_environment() {
    log_info "Setting up development environment..."

    # Install global npm packages if npm is available
    if command_exists npm; then
        log_info "Installing global npm packages..."
        npm install -g yarn pnpm typescript @angular/cli 2>/dev/null || true
    fi

    # Setup Python virtualenvwrapper if Python is available
    if command_exists python3; then
        log_info "Setting up Python virtualenvwrapper..."
        pip3 install virtualenvwrapper 2>/dev/null || true
    fi

    log_success "Development environment setup completed"
}

# Validate installation
validate_setup() {
    log_info "Validating installation..."

    local errors=0

    # Check Homebrew
    if ! command_exists brew; then
        log_error "Homebrew not found!"
        ((errors++))
    else
        log_success "Homebrew: $(brew --version | head -1)"
    fi

    # Check dotfiles command
    if ! command_exists dotfiles; then
        log_error "dotfiles command not found!"
        ((errors++))
    else
        log_success "dotfiles command: Available"
    fi

    # Check package counts (using clean Brewfile)
    local formula_count=$(brew list --formula 2>/dev/null | wc -l)
    local cask_count=$(brew list --cask 2>/dev/null | wc -l)

    if (( formula_count < 50 )); then
        log_warning "Only $formula_count formulas installed (expected ~60 from clean Brewfile)"
    else
        log_success "Formulas: $formula_count installed"
    fi

    if (( cask_count < 10 )); then
        log_warning "Only $cask_count casks installed (expected ~16)"
    else
        log_success "Casks: $cask_count installed"
    fi

    # Check critical applications
    local critical_apps=("Emacs" "iTerm2" "Karabiner-Elements")
    for app in "${critical_apps[@]}"; do
        if [[ -d "/Applications/$app.app" ]]; then
            log_success "$app: Installed"
        else
            log_warning "$app: Not found in /Applications"
        fi
    done

    # Check services
    if brew services list | grep -q "yabai.*started"; then
        log_success "Yabai service: Running"
    else
        log_warning "Yabai service: Not running"
    fi

    if brew services list | grep -q "skhd.*started"; then
        log_success "SKHD service: Running"
    else
        log_warning "SKHD service: Not running"
    fi

    if (( errors > 0 )); then
        log_error "Validation found $errors critical errors"
        return 1
    else
        log_success "Validation completed successfully"
        return 0
    fi
}

# Final cleanup
cleanup() {
    log_info "Running final cleanup..."

    # Update Homebrew
    brew update

    # Upgrade packages
    brew upgrade

    # Cleanup old versions
    brew cleanup

    # Run brew doctor
    brew doctor

    log_success "Cleanup completed"
}

# Main execution
main() {
    log_info "Starting macOS setup bootstrap..."
    log_info "This may take some time. Please be patient!"

    install_xcode_clt
    install_homebrew
    setup_dotfiles
    install_packages
    apply_macos_defaults
    setup_karabiner
    setup_window_manager
    setup_dev_environment
    validate_setup
    cleanup

    log_success "🎉 macOS setup completed successfully!"
    log_info "Your dotfiles are now managed by the 'dotfiles' command."
    log_info "You may need to restart your Mac for all changes to take effect."
    log_info "Don't forget to:"
    log_info "  - Sign into iCloud"
    log_info "  - Configure your Git credentials"
    log_info "  - Set up SSH keys"
    log_info "  - Install any additional apps from the Mac App Store"
    log_info ""
    log_info "Useful dotfiles commands:"
    log_info "  dotfiles --list     # Show managed files"
    log_info "  dotfiles --sync     # Update symlinks"
    log_info "  dotfiles --check    # Check for issues"
}

# Run main function
main "$@"