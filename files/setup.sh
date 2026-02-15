#!/bin/bash
# Emacs Ultimate Setup Script
# For Common Lisp Development with CLOG

echo "=================================================="
echo "  Emacs Ultimate Setup for Common Lisp + CLOG"
echo "=================================================="
echo ""

# Check if running on Linux
if [[ ! "$OSTYPE" =~ ^linux ]]; then
    echo "⚠️  This script is designed for Linux. Adapt paths for your OS."
    echo ""
fi

# 1. Backup existing config
if [ -f ~/.emacs.d/init.el ]; then
    echo "📦 Backing up existing init.el to init.el.backup..."
    cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup
fi

# 2. Create .emacs.d directory
echo "📁 Creating ~/.emacs.d directory..."
mkdir -p ~/.emacs.d

# 3. Copy init.el
echo "📝 Installing init.el..."
cp init.el ~/.emacs.d/init.el

# 4. Create necessary directories
echo "📁 Creating project directories..."
mkdir -p ~/projects
mkdir -p ~/org
mkdir -p ~/org-roam
mkdir -p ~/Music

# 5. Check for SBCL
echo ""
echo "🔍 Checking for Common Lisp (SBCL)..."
if command -v sbcl &> /dev/null; then
    echo "✅ SBCL found: $(sbcl --version)"
else
    echo "❌ SBCL not found. Please install:"
    echo "   Ubuntu/Debian: sudo apt install sbcl"
    echo "   Arch: sudo pacman -S sbcl"
    echo "   macOS: brew install sbcl"
fi

# 6. Check for git
echo ""
echo "🔍 Checking for Git..."
if command -v git &> /dev/null; then
    echo "✅ Git found: $(git --version)"
else
    echo "❌ Git not found. Please install:"
    echo "   Ubuntu/Debian: sudo apt install git"
fi

# 7. Check for PostgreSQL (optional)
echo ""
echo "🔍 Checking for PostgreSQL..."
if command -v psql &> /dev/null; then
    echo "✅ PostgreSQL found: $(psql --version)"
else
    echo "⚠️  PostgreSQL not found (optional for database work)"
    echo "   Ubuntu/Debian: sudo apt install postgresql"
fi

# 8. Check for mpv (for music)
echo ""
echo "🔍 Checking for mpv (music player)..."
if command -v mpv &> /dev/null; then
    echo "✅ mpv found: $(mpv --version | head -n1)"
else
    echo "⚠️  mpv not found (optional for music)"
    echo "   Ubuntu/Debian: sudo apt install mpv"
fi

# 9. Install Quicklisp (optional but recommended)
echo ""
echo "🔍 Checking for Quicklisp..."
if [ -d ~/quicklisp ]; then
    echo "✅ Quicklisp found"
else
    echo "⚠️  Quicklisp not found (highly recommended for Common Lisp)"
    echo "   Install with:"
    echo "   curl -O https://beta.quicklisp.org/quicklisp.lisp"
    echo "   sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit"
fi

echo ""
echo "=================================================="
echo "  Installation Complete!"
echo "=================================================="
echo ""
echo "Next steps:"
echo "1. Start Emacs - packages will auto-install (5-10 min)"
echo "2. Run: M-x all-the-icons-install-fonts"
echo "3. Restart Emacs"
echo "4. Press C-h t for the built-in tutorial"
echo "5. Read the reference guide for all keybindings"
echo ""
echo "Quick access:"
echo "  C-c i     - Open init.el for customization"
echo "  M-x sly   - Start Common Lisp REPL"
echo "  C-x g     - Git status (Magit)"
echo "  C-c c     - Capture note"
echo "  C-c n f   - Find/create org-roam note"
echo ""
echo "Happy coding! 🚀"
echo ""
