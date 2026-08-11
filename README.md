# .doom.d
Personal Doom Emacs configuration with modular customizations.

## TODOs
M-w in visual/insert mode works differently when M-y.
comments that starts with "//" in .cpp cannot be indented with <tab>


## Key Features

- **IJKL navigation model** - Arrow-key style movement (see Keybindings section)
- Completion framework (Consult, Vertico, Orderless, Company)
- LSP integration with clangd for C/C++ development
- Dape debugging support (native lldb-dap)
- Custom light theme (`professional-theme`)
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
- `init-functions.el` - General-purpose utility commands (navigation, selection, file operations)
- `init-utils.el` - Completion & search package config (consult, marginalia, orderless, company, rg)
- `init-modeline.el` - Doom Modeline visual configuration (faces, font scaling, icons)

### Development Tools
- `init-lsp.el` - LSP-UI configuration (clangd setup for C/C++)
- `init-dape.el` - Dape debugger configuration (native lldb-dap)
- `init-glsl.el` - GLSL shader support
- `init-projectile.el` - Projectile settings (indexing, caching, compile commands)
- `init-tramp.el` - Remote file editing configuration (TRAMP)

### Keybindings (Modular)
- `init-keybinds-common.el` - Common keybindings shared across all modes (IJKL model)
- `init-keybinds-modes.el` - Mode-specific bindings (vterm, vertico, help, image, cc-mode)
- `init-keybinds-org.el` - Org-mode specific keybindings
- `init-keybinds-md.el` - Markdown-mode specific keybindings
- `init-keybinds-treemacs.el` - Treemacs specific keybindings
- `init-keybinds-magit.el` - Magit keybindings adapted to the IJKL model

### Other Modules
- `init-org.el` - Org-mode configuration (TOC, LaTeX preview, source blocks)

### Themes
- `professional-theme` package (see `packages.el`) - Light theme with white background (active)

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
