## InCollege (COBOL)

### Prereqs
- VS Code with the **Dev Containers** extension (or GitHub Codespaces). The workspace is meant to be opened inside the provided Docker dev container so that `cobc` and tasks are already configured.

### Open in the dev container
1) Open the repo in VS Code.
2) If prompted, select **Reopen in Container**. Otherwise: Command Palette → **Dev Containers: Reopen in Container**.
3) Wait for the container to build; COBOL toolchain and tasks will be available inside it.

### Build and run (VS Code tasks)
- Default build: press `Ctrl+Shift+B` (runs task **COBOL: Build InCollege**) to produce `/workspace/bin/InCollege`.
- Run: Command Palette → **Run Task** → **COBOL: Run InCollege** to execute `/workspace/bin/InCollege`.

### Using the program
- Input is read from `INPUT.DAT`; output is written to `OUTPUT.DAT` and also displayed.
- Top-level menu: Log In, Create a new account, Logout.
- After successful login: options for job search, find someone, learn a new skill (all show "under construction"), plus Logout.
- Account creation enforces: username 1–12 chars; password 8–12 chars with upper, digit, and special char.

### Important files
- [src/InCollege.cob](src/InCollege.cob) — entry point, calls `INCOLLEGE-START`.
- [src/menu/InitialMenu.cob](src/menu/InitialMenu.cob) — menus, login, post-login options, and inlined create-account logic.
- [src/account/CREATEACCOUNT.cob](src/account/CREATEACCOUNT.cob) — standalone create-account program (kept for reuse/testing).
- [USERS.DAT](USERS.DAT) — line-sequential user store; overwritten on account creation.
- [INPUT.DAT](INPUT.DAT) / `INPUT_*.dat` — test inputs; the program consumes lines in order.

### Troubleshooting
- **EOF during menus**: If `INPUT.DAT` runs out, you will see messages like "No input received." or returns to previous menu. Add enough lines to drive the flow you want.
- **Accounts not found / stale users**: `USERS.DAT` is rewritten on account creation. If tests expect a clean state, reset `USERS.DAT` to your baseline before rerunning, as the max number of accounts is limited.
- **Password rejected**: Ensure length 8–12, includes at least one uppercase, one digit, one special (non-space, non-alphanumeric).
- **Build errors**: Verify you are inside the dev container so `cobc` is on PATH. Reopen in container if needed. Then rerun the build task.
- **Executable missing**: Rebuild with the task or run the terminal build command above to recreate `/workspace/bin/InCollege`.

