# Getting Started with Lisp Koans in Emacs with SLY

Great choice! The Lisp Koans are an excellent way to learn Common Lisp. Here's how to work through them in Emacs with SLY.

## Initial Setup

First, make sure you have the Lisp Koans cloned:
```bash
git clone https://github.com/google/lisp-koans.git
cd lisp-koans
```

## Essential Emacs/SLY Workflow

### 1. Starting SLY
- Open Emacs: `emacs`
- Start SLY: `M-x sly` (that's Alt+x, then type "sly" and press Enter)
- This launches SBCL and connects SLY to it

### 2. Opening the Koans in Treemacs
- Toggle Treemacs: `M-x treemacs` or the keybinding you've set
- Navigate to your `lisp-koans` folder
- You'll see files like `koans.lisp`, `contemplate.lisp`, and folders like `koans/`

### 3. Key Emacs Bindings You'll Need

**Navigation:**
- `C-x C-f`: Find (open) file
- `C-x b`: Switch buffer
- `C-x k`: Kill (close) buffer
- `C-x 2`: Split window horizontally
- `C-x 3`: Split window vertically
- `C-x o`: Switch to other window
- `C-x 1`: Close all other windows (maximize current)

**Basic Editing:**
- `C-g`: Cancel/quit current command (your panic button!)
- `C-/` or `C-_`: Undo
- `C-s`: Search forward
- `C-r`: Search backward

### 4. Running the Koans

**Method 1: From Treemacs**
1. In Treemacs, navigate to `contemplate.lisp`
2. Press `RET` (Enter) to open it
3. With cursor in the file: `C-c C-k` to compile/load the file
4. In the SLY REPL buffer: `(contemplate)` to run the koans

**Method 2: From REPL directly**
1. Switch to the SLY REPL buffer: `C-c C-z`
2. Load the file:
   ```lisp
   (load "contemplate.lisp")
   (contemplate)
   ```

### 5. Working Through Koans - The Core Workflow

The koans will show you an error with a line number. Here's your workflow:

1. **Note the failing koan** (e.g., "Meditation on ASSERTS is damaged")
2. **Open the relevant file** from Treemacs (e.g., `koans/asserts.lisp`)
3. **Split your window** (`C-x 3`) so you can see code and REPL side-by-side
4. **Find the broken assertion** (look for `___` placeholders)
5. **Edit the code** - replace `___` with the correct answer
6. **Evaluate your fix**:
   - Put cursor after the closing paren of the defun
   - Press `C-M-x` (evaluates the current top-level form)
   - OR select the form and use `C-c C-r` (evaluates region)
7. **Re-run the koans**: Switch to REPL (`C-c C-z`) and run `(contemplate)` again

### 6. Super Useful SLY Commands

**Evaluation:**
- `C-c C-k`: Compile and load entire current file
- `C-M-x`: Evaluate top-level form at point (the defun you're in)
- `C-c C-r`: Evaluate selected region
- `C-c C-c`: Compile the defun at point

**Navigation & Help:**
- `M-.`: Jump to definition (works on functions, variables)
- `M-,`: Jump back
- `C-c C-d d`: Describe symbol (shows documentation)
- `C-c C-d h`: HyperSpec lookup (opens CL spec in browser)
- `C-c C-z`: Switch between code and REPL

**REPL:**
- `C-c C-z`: Jump to REPL from code
- In REPL, use `↑`/`↓` for history
- `C-c C-o`: Clear last REPL output

### 7. Practical Example Walkthrough

Let's say you start the koans and see:
```
Meditation on ASSERTS is damaged.
```

**Step-by-step:**

1. In Treemacs, open `koans/asserts.lisp`
2. Split window: `C-x 3`
3. You see something like:
   ```lisp
   (define-test assert-truth
     (let ((true-value ___)
           (false-value ___))
       (assert-true true-value)
       (assert-false false-value)))
   ```
4. Replace the first `___` with `t` and second with `nil`
5. Put cursor after the closing paren of the `define-test`
6. Press `C-M-x` to evaluate it
7. Switch to REPL: `C-c C-z`
8. Run: `(contemplate)`
9. See if you fixed it! If not, repeat.

### 8. Helpful Tips

**Paredit/Smartparens:** You probably have one of these for balanced parentheses:
- `C-M-f`: Move forward by s-expression
- `C-M-b`: Move backward by s-expression
- `C-M-u`: Move up one level
- `C-M-d`: Move down one level

**When Things Go Wrong:**
- If SLY seems stuck: `C-c C-b` (interrupt Lisp)
- If completely stuck: `M-x sly-quit-lisp` then `M-x sly` to restart
- Remember `C-g` cancels most operations

**Keeping Track:**
- Keep one window with Treemacs (file tree)
- One window with current koan file
- One window with REPL
- Use `C-x o` to cycle between them

### 9. Recommended Window Layout

```
┌─────────────┬──────────────────┐
│             │                  │
│  Treemacs   │  Current Koan    │
│  (files)    │  (editing here)  │
│             │                  │
├─────────────┴──────────────────┤
│                                │
│        SLY REPL                │
│                                │
└────────────────────────────────┘
```

Set this up with:
1. `C-x 3` to split vertically
2. `C-x 2` in right window to split horizontally
3. `M-x treemacs` in left window
4. Open koan file in top-right
5. `C-c C-z` in bottom-right to get REPL

## Your First Session Checklist

1. ☐ `emacs` - Start Emacs
2. ☐ `M-x sly` - Start SLY
3. ☐ `M-x treemacs` - Open file browser
4. ☐ Navigate to lisp-koans folder in Treemacs
5. ☐ Open `contemplate.lisp`, compile with `C-c C-k`
6. ☐ Run `(contemplate)` in REPL
7. ☐ Open first failing koan file
8. ☐ Fix the `___`, evaluate with `C-M-x`
9. ☐ Re-run `(contemplate)`
10. ☐ Repeat until enlightened!

## Quick Reference Card

### Most Important Commands (Learn These First!)
| Command | What It Does |
|---------|--------------|
| `C-c C-k` | Load/compile current file |
| `C-M-x` | Evaluate current form (defun) |
| `C-c C-z` | Jump to REPL |
| `M-.` | Jump to definition |
| `M-,` | Jump back |
| `C-g` | Cancel current operation |

### File & Buffer Management
| Command | What It Does |
|---------|--------------|
| `C-x C-f` | Open file |
| `C-x C-s` | Save file |
| `C-x b` | Switch buffer |
| `C-x k` | Kill (close) buffer |

### Window Management
| Command | What It Does |
|---------|--------------|
| `C-x 2` | Split window horizontally |
| `C-x 3` | Split window vertically |
| `C-x o` | Switch to other window |
| `C-x 1` | Close other windows |
| `C-x 0` | Close current window |

### Evaluation & Compilation
| Command | What It Does |
|---------|--------------|
| `C-c C-k` | Compile/load file |
| `C-M-x` | Eval top-level form |
| `C-c C-r` | Eval region |
| `C-c C-c` | Compile defun at point |
| `C-c C-e` | Eval last expression |

### Navigation
| Command | What It Does |
|---------|--------------|
| `C-M-f` | Forward s-expression |
| `C-M-b` | Backward s-expression |
| `C-M-u` | Up one level |
| `C-M-d` | Down one level |
| `C-M-a` | Beginning of defun |
| `C-M-e` | End of defun |

### Help & Documentation
| Command | What It Does |
|---------|--------------|
| `C-c C-d d` | Describe symbol |
| `C-c C-d h` | HyperSpec lookup |
| `C-c C-d a` | Apropos (search) |
| `C-h k` | Describe key |
| `C-h f` | Describe function |

### REPL Specific
| Command | What It Does |
|---------|--------------|
| `C-c C-z` | Switch to REPL |
| `↑` / `↓` | Navigate history |
| `C-c C-o` | Clear last output |
| `C-c C-b` | Interrupt Lisp |
| `M-x sly-quit-lisp` | Quit SLY |

### Debugging
| Command | What It Does |
|---------|--------------|
| `v` | In debugger: view source |
| `q` | In debugger: quit |
| `a` | In debugger: abort |
| `c` | In debugger: continue |
| `0-9` | In debugger: invoke restart |

## Common Emacs Notation Guide

- `C-` means hold Control
- `M-` means hold Alt (or ESC then the key)
- `C-x` means Control+x
- `C-M-x` means Control+Alt+x (hold both)
- `RET` means Enter/Return key
- Multiple commands: `C-x C-f` means Control+x, then Control+f

## Troubleshooting

### SLY Won't Start
- Make sure SBCL is installed: `sbcl --version` in terminal
- Check your Emacs config has SLY configured
- Try `M-x sly-connect` if SLY is already running

### Can't Evaluate Code
- Make sure SLY is running (`M-x sly` if not)
- Make sure you're in a Lisp file (.lisp extension)
- Check the bottom of Emacs - it should show the mode (Lisp/SLY)

### Wrong Directory
- In REPL, check current directory: `(uiop:getcwd)`
- Change directory: `(uiop:chdir "/path/to/lisp-koans/")`
- Or start Emacs from the lisp-koans directory

### Parentheses Mismatch
- Use `M-x check-parens` to find unbalanced parens
- Most Emacs Lisp setups highlight matching parens
- When in doubt, count your parens or use `C-M-f`/`C-M-b` to navigate

### Accidentally Closed Something Important
- `C-/` to undo
- `C-x b *sly-repl*` to get back to REPL
- `M-x sly` to restart if you quit SLY entirely

## Learning Path

1. **Week 1:** Get comfortable with basic navigation and evaluation
   - Focus on `C-c C-k`, `C-M-x`, `C-c C-z`
   - Complete the first few koans (asserts, nil-false, truth)

2. **Week 2:** Learn the inspection commands
   - Use `M-.` to explore definitions
   - Use `C-c C-d d` to read documentation
   - Complete koans on symbols and lists

3. **Week 3:** Master window management
   - Set up your ideal layout
   - Get comfortable with `C-x 1`, `C-x 2`, `C-x 3`, `C-x o`
   - Complete more advanced koans

4. **Week 4+:** Explore advanced features
   - Learn the debugger
   - Experiment with macrostep
   - Start your own Lisp projects!

## Resources

- **Lisp Koans GitHub:** https://github.com/google/lisp-koans
- **SLY Manual:** https://joaotavora.github.io/sly/
- **Common Lisp HyperSpec:** http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
- **Practical Common Lisp:** http://www.gigamonkeys.com/book/ (free online book)
- **Emacs Tutorial:** In Emacs, run `C-h t` for built-in tutorial

## Final Tips

1. **Don't try to memorize everything** - keep this guide handy and reference it
2. **Muscle memory takes time** - the keybindings will become natural with practice
3. **Experiment!** - The worst that can happen is you press `C-g` or restart SLY
4. **Save often** - `C-x C-s` becomes automatic quickly
5. **Use the checklist** - Follow it for your first few sessions until it's routine
6. **Ask for help** - The Common Lisp and Emacs communities are very friendly

Remember: Every expert was once a beginner who didn't give up. You've got this!

---

*Last updated: February 2026*
*For corrections or suggestions, feel free to modify this guide as you learn!*
