## InCollege (COBOL)

### Overview
InCollege is a menu-driven COBOL application that now includes the full project workflow:
- account creation and login
- profile create, edit, and view
- full-name user search
- sending, viewing, accepting, and rejecting connection requests
- viewing established network connections
- sending private messages to established connections
- placeholder "View My Messages" menu flow for Week 8
- basic profile storage (plus education/experience backing files)

The compiled executable is `/workspace/bin/InCollege` and is built from `src/InCollege.cob`.

### Prerequisites
- Open the project in the provided dev container, or use an environment with `cobc` installed.

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

The program writes the exact displayed output to:
- `InCollege-Output.txt`

Run:
```bash
/workspace/bin/InCollege
```

### Main Menus
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
- `7. View My Network`
- `8. Messages`

Messages submenu:
- `1. Send a New Message`
- `2. View My Messages`
- `3. Back to Main Menu`

### Week 5 Connection Behavior
Pending request management (`4. View My Pending Connection Requests`):
- displays each pending request for the logged-in user
- prompts for `A` (accept) or `R` (reject) per request
- on accept:
  - removes the request from pending storage
  - adds the pair to established connections storage
  - prints confirmation
- on reject:
  - removes the request from pending storage
  - does not create an established connection
  - prints confirmation
- prints a processed summary for accepted/rejected totals

Network display (`7. View My Network`):
- reads established connections and lists users connected to the logged-in user
- displays at least full name (and also University/Major when profile exists)
- prompts `Enter 0 to return to post-login menu.`

### Data Files
The program uses line-sequential `.DAT` files in the workspace root:
- `USERS.DAT`
- `PROFILES.DAT`
- `CONNECTIONS.DAT`
- `ESTABLISHED.DAT`
- `EDUCATION.DAT`
- `EXPERIENCE.DAT`
- `MESSAGES.DAT`

Connection file roles:
- `CONNECTIONS.DAT`: pending requests only (`requester`, `recipient`, status field used as pending workflow input)
- `ESTABLISHED.DAT`: established permanent connections (`user1`, `user2`)
- `MESSAGES.DAT`: private message storage (`sender`, `recipient`, `content`, `timestamp`)

### I/O Rules
- Input is read from `InCollege-Input.txt` via file reads (no interactive keyboard input required).
- Every line printed to the screen is also written to `InCollege-Output.txt` through shared print logic.
- This applies to Week 8 messaging flows as well, including recipient prompts, message prompts, confirmations, and validation errors.

### Automated Tests
Run all Epic 8 tests:
```bash
./run_tests.sh /workspace/Tests/Epic8
```

Run a specific test root:
```bash
./run_tests.sh /workspace/Tests/Epic8/NM-154_SendMessage
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
- `src/MESSAGING_SRC.cpy`: messages submenu, send-message flow, and recipient validation
- `src/VIEWREQ_SRC.cpy`: pending request view + accept/reject processing
- `src/VIEWNET_SRC.cpy`: established network display logic
- `run_tests.sh`: automated test runner
- `Tests/Epic8/`: Epic 8 test cases and isolated fixture `.DAT` files

### Build And Try Week 8 Messaging Flow
1. Build:
```bash
mkdir -p bin
cobc -x -free -o bin/InCollege src/InCollege.cob
```

2. Optional fixture setup for a sample Week 8 run:
```bash
cp Tests/Epic8/USERS.DAT USERS.DAT
: > PROFILES.DAT
: > CONNECTIONS.DAT
: > ESTABLISHED.DAT
: > EDUCATION.DAT
: > EXPERIENCE.DAT
: > JOBS.DAT
: > MESSAGES.DAT
```

Run a specific test root:
```bash
./run_tests.sh /workspace/Tests/Epic9
```

4. Review output:
- screen output
- `/workspace/InCollege-Output.txt` (should match screen output line-for-line)
- `/workspace/MESSAGES.DAT` for persisted sender/recipient/content/timestamp records
