# Zoïe Street's dotfiles

Personal configuration files

---

## Known Issues & Troubleshooting

### Emacs in terminal shows `Failed select. Operation timed out`

In Cocoa (NS) GUI builds of Emacs, `select` calls are handled by `ns_select_1` in `src/nsterm.m`.
When run in terminal mode (`-nw`), whenever Emacs goes idle for 1–2 seconds, `ns_select_1`
catches the routine idle timeout and unconditionally calls:

```c
report_file_error ("Failed select", Qnil);
```

**Fix:** Link the native Homebrew CLI build of Emacs to `PATH`:

```bash
brew link --overwrite emacs
```

---

### Vim is slow to start due to loading X sessions

Vim may experience slow startup times if `SESSION_MANAGER` is set and attempting X session negotiation.

**Fix:** Reset `SESSION_MANAGER` in your shell environment ([reference](https://github.com/christoomey/dotfiles/issues/13#issuecomment-740943680)):

```bash
export SESSION_MANAGER=
```
