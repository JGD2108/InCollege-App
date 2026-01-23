InCollege - Run Instructions

**Requirements**
- `cobc` (GnuCOBOL) installed on the system
- Linux environment (devcontainer available in repo)

**Build (CLI)**
Compile the main program and its submodule (creates `/workspace/bin/InitialMenu`):

```bash
mkdir -p /workspace/bin
cobc -x -free -o /workspace/bin/InitialMenu /workspace/src/InitialMenu.cob /workspace/src/CREATEACCOUNT.cob
```

**Run (CLI)**
Run the compiled binary from the workspace root:

```bash
/workspace/bin/InitialMenu
```

The program reads input lines from `INPUT.DAT` (in the workspace root). Test inputs are provided as `INPUT_valid.dat`, `INPUT_user_empty.dat`, `INPUT_pass_invalid.dat`, `INPUT_existing_user.dat`, etc. To run a test, overwrite `INPUT.DAT` with one of these files and then run the binary.

Example:

```bash
cp INPUT_valid.dat INPUT.DAT
/workspace/bin/InitialMenu
```

**Run in VS Code (Tasks)**
- Press `Ctrl+Shift+B` to run the default build task: this compiles `InitialMenu` and `CREATEACCOUNT` into `/workspace/bin/InitialMenu` (does not execute).
- To execute the program from VS Code: Command Palette → `Run Task` → choose `COBOL: Run InitialMenu` (runs `/workspace/bin/InitialMenu`).

**Behavior Notes**
- Account creation rewrites `USERS.DAT` from the in-memory table; tests that create users will change `USERS.DAT`. If you need a stable baseline, restore `USERS.DAT` before running other tests.
- Login supports unlimited attempts. On failed username/password the program prints a unified message: "Incorrect username / password, please try again" and prompts again.

**Files of interest**
- `src/InitialMenu.cob` — main program and menu flow
- `src/CREATEACCOUNT.cob` — account-creation / validation logic
- `USERS.DAT` — users data file (line-sequential)
- `INPUT_*.dat` — example test inputs

If you want, I can add a small shell script to run the whole test suite and restore `USERS.DAT` between tests. Would you like that?
