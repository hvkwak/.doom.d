# .doom.d

Personal Doom Emacs configuration with modular customizations.

## Key Features

- **IJKL navigation model** - Arrow-key style movement (see Keybindings section)
- Completion framework (Consult, Vertico, Orderless, Company)
- LSP integration with clangd for C/C++ development
- DAP/Dape debugging support (GDB/LLDB)
- Custom light theme (`professional`)
- Org-mode and Markdown configurations
- Remote development support (TRAMP)

## Keybinding Model

This config uses an **IJKL movement model** instead of standard Vim HJKL:

| Key | Function | Standard Vim |
|-----|----------|--------------|
| `i` | Move up | Enter insert mode |
| `k` | Move down | Move up |
| `j` | Move left | Move down |
| `l` | Move right | Move right |
| `h` | Switch tabs | Move left |

Additional remaps:
- `M-i` - Enter insert mode (in normal mode)
- `w` - Yank (copy), `y` - Paste
- `u` - Beginning of line, `o` - End of line
- `z` - Undo

## File Organization

Configuration is modularized in `lisp/` directory:

### Core Configuration
- `init-ui.el` - Visual appearance (theme, fonts, faces, cursors, scrolling)
- `init-editor.el` - Editor settings (performance, tabs, indentation, evil cursor)
- `init-behavior.el` - Behavioral modifications (advice, hooks, minor modes)
- `init-functions.el` - Utility functions (navigation, selection, file operations)

### Development Tools
- `init-lsp.el` - LSP configuration (lsp-ui, clangd)
- `init-dape.el` - Dape debugger configuration
- `init-dap.el` - DAP UI and multi-monitor frame management
- `init-dap-gdb.el` - GDB debugger configuration
- `init-dap-lldb.el` - LLDB debugger configuration
- `init-glsl.el` - GLSL shader support

### Keybindings (Modular)
- `init-keybinds-common.el` - Common keybindings shared across all modes (IJKL model)
- `init-keybinds-modes.el` - Mode-specific bindings (vterm, vertico, help, image, cc-mode)
- `init-keybinds-org.el` - Org-mode specific keybindings
- `init-keybinds-md.el` - Markdown-mode specific keybindings
- `init-keybinds-treemacs.el` - Treemacs specific keybindings

### Other Modules
- `init-completion.el` - Completion framework (Consult, Marginalia, Orderless, Company, rg)
- `init-org.el` - Org-mode configuration (LaTeX preview, TOC, source blocks)
- `init-vterm.el` - Terminal emulator configuration
- `init-llm.el` - LLM integration templates (GPTel, Claude Code IDE)
- `init-projectile.el` - Project management settings
- `init-tramp.el` - Remote file editing configuration

### Themes
- `themes/professional-theme.el` - Light theme with white background (active)
- `themes/leuven-theme.el` - Alternative light theme
- `themes/sunny-day-theme.el` - Alternative light theme

## Installation

1. Clone to `~/.doom.d/`:
   ```bash
   git clone <your-repo> ~/.doom.d
   ```

2. Run Doom sync:
   ```bash
   doom sync
   ```

3. Restart Emacs

## Dependencies

- JetBrains Mono font (for `doom-font`)
- clangd (for C/C++ LSP support)
- ripgrep (for `rg` package)
- LaTeX + dvipng (for org-mode LaTeX preview)
