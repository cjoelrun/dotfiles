#!/bin/bash
# Script to clean and rebuild straight.el packages

echo "Cleaning straight build directory..."
rm -rf straight/build

echo "Removing straight build cache..."
rm -f straight/build-cache.el

echo "Done! Please restart Emacs to rebuild packages."
echo "Straight.el will automatically rebuild all packages on next startup."