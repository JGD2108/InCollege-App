## InCollege (COBOL)

### Overview
InCollege is a menu-driven COBOL application that supports:
- account creation and login
- profile search by full name
- sending connection requests
- viewing pending connection requests
- basic profile storage (plus education/experience backing files)

The compiled executable is `/workspace/bin/InCollege` and is built from `src/InCollege.cob`.

### Prerequisites
- Open the project in the provided dev container (or equivalent environment with `cobc` installed).

### Build
Use either method:
1. VS Code task: `COBOL: Build Single-file InCollege`
2. Terminal:
```bash
mkdir -p bin
cobc -x -free -o bin/InCollege src/InCollege.cob
```

### Run
The program reads scripted input from:
- `InCollege-Input.txt`

And writes output to:
- `InCollege-Output.txt`

Run:
```bash
/workspace/bin/InCollege
```

### Current Menus
Top-level menu:
- `1. Log In`
- `2. Create a new account`
- `3. Logout`

Post-login menu:
- `0. Create/Edit your profile`
- `1. Search for a job`
- `2. Find someone you know`
- `3. Learn a new skill`
- `4. View My Pending Connection Requests`
- `5. Logout`
- `6. View My Profile`

### Data Files
The program uses line-sequential `.DAT` files in the workspace root:
- `USERS.DAT`
- `PROFILES.DAT`
- `CONNECTIONS.DAT`
- `EDUCATION.DAT`
- `EXPERIENCE.DAT`

### Automated Tests
Run all Epic 4 tests:
```bash
./run_tests.sh
```

Run a specific test root:
```bash
./run_tests.sh /workspace/Tests/Epic4
```

Test runner behavior:
- discovers test cases by `InCollege-Input.txt` (or `INPUT.DAT`)
- writes actual outputs into each case as `ACTUAL-InCollege-Output.txt`
- reseeds `.DAT` fixtures per test for isolation
- loads epic-level fixtures from files like `Tests/Epic4/USERS.DAT`, `Tests/Epic4/PROFILES.DAT`, etc.
- supports per-test fixture overrides by placing `.DAT` files directly in a test case folder

### Important Files
- `src/InCollege.cob`: primary source (includes program units and copied source snippets)
- `src/SendRequest.cob`: connection request validation/save logic
- `src/VIEWREQ_SRC.cpy`: pending request display logic
- `run_tests.sh`: automated test runner
- `Tests/Epic4/`: Epic 4 test cases and fixture `.DAT` files
