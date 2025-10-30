# .doom.d

Personal Doom Emacs configuration with modular customizations.

## Key Features

- Custom keybindings optimized for custom layout
- Completion framework (Consult, Vertico, Company)
- LSP/DAP integration for debugging (GDB/LLDB) with multi-monitor support
- LLM integration (GPTel, Claude Code IDE)
- Org-mode and Markdown configurations
- Remote development support (TRAMP)

## File Organization

Configuration is modularized in `lisp/` directory:

### Core Configuration
- `init-ui.el` - Visual appearance (theme, fonts, faces, cursors, scrolling)
- `init-editor.el` - Editor behavior (tabs, indentation, evil cursor, flycheck)
- `init-functions.el` - Utility functions (navigation, selection, file operations, jump lists)

### Development Tools
- `init-lsp.el` - LSP configuration (lsp-ui, clangd)
- `init-dap.el` - General DAP UI and multi-monitor frame management
- `init-dap-gdb.el` - GDB debugger configuration
- `init-dap-lldb.el` - LLDB debugger configuration

### Keybindings (Modular)
- `init-keybinds.el` - Core global and evil keybindings
- `init-keybinds-modes.el` - Mode-specific bindings (vterm, vertico, help, image, cc-mode)
- `init-keybinds-common.el` - Shared writing mode keybindings
- `init-keybinds-org.el` - Org-mode specific keybindings
- `init-keybinds-md.el` - Markdown-mode specific keybindings
- `init-keybinds-treemacs.el` - Treemacs specific keybindings

### Other Modules
- `init-completion.el` - Completion framework (Consult, Vertico, Orderless, Company)
- `init-org.el` - Org-mode configuration (LaTeX preview, TOC, source blocks)
- `init-llm.el` - LLM integration (GPTel, Claude Code IDE)
- `init-projectile.el` - Project management settings
- `init-tramp.el` - Remote file editing configuration

## Installation

1. Clone to `~/.doom.d/`:
   ```bash
   git clone <your-repo> ~/.doom.d
   ```

2. Ensure you have the new modules loaded in your main config

3. Run Doom sync:
   ```bash
   doom sync
   ```
