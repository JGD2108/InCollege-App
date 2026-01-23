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
