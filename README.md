## InCollege (COBOL)

### Overview
InCollege is a menu-driven COBOL application that now includes the full project workflow:
- account creation and login
- profile create, edit, and view
- full-name user search
- sending, viewing, accepting, and rejecting connection requests
- viewing established network connections
- job posting and job browsing
- basic messaging, including sending messages and viewing received messages

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
- `8. Messaging`

Messages submenu:
- `1. Send a New Message`
- `2. View My Messages`
- `3. Back to Main Menu`

Job submenu:
- `1. Post a Job/Internship`
- `2. Browse Jobs/Internships`
- `3. Back to Main Menu`

### Messaging Behavior
Messaging is the completed Week 8 and Week 9 feature set.

Send message flow:
- the sender chooses `8` from the post-login menu, then `1` in the Messages menu
- the recipient must be a user in the sender's established network
- the message body accepts up to 200 characters
- messages are persisted to `MESSAGES.DAT`
- each saved message includes sender, recipient, content, and timestamp data

View messages flow:
- the user chooses `8` from the post-login menu, then `2` in the Messages menu
- all messages addressed to the logged-in user are displayed
- each message shows:
  - sender username
  - message content
  - timestamp
- if there are no messages, the program prints `You have no messages at this time.`
- the current alpha does not track read/unread state

### File I/O Rules
- All user input is read from `InCollege-Input.txt`.
- The same lines shown on screen are also written to `InCollege-Output.txt`.
- Input is echoed into the output file because the shared read/display flow logs the scripted selections for test verification.
- This file-based behavior is used for login, profiles, connection requests, jobs, and messaging.

### Data Files
The application uses line-sequential files in the workspace root:
- `USERS.DAT`: usernames and passwords
- `PROFILES.DAT`: core profile details
- `EDUCATION.DAT`: education entries
- `EXPERIENCE.DAT`: experience entries
- `CONNECTIONS.DAT`: pending connection requests
- `ESTABLISHED.DAT`: accepted network connections
- `JOBS.DAT`: posted jobs and internships
- `MESSAGES.DAT`: saved messages with sender, recipient, content, and timestamp

### Important Files
- `src/InCollege.cob`: main single-file build target
- `src/MESSAGING_SRC.cpy`: messaging menu, send flow, and view-my-messages flow
- `src/VIEWREQ_SRC.cpy`: pending request processing
- `src/VIEWNET_SRC.cpy`: established network display
- `src/JOBS_SRC.cpy`: job posting menu flow
- `src/BROWSEJOBS_SRC.cpy`: job listing and detail view logic
- `run_tests.sh`: automated test runner
- `Tests/Epic9/`: Week 9 messaging fixtures and regression tests

### Automated Tests
Build first:
```bash
mkdir -p bin
cobc -x -free -o bin/InCollege src/InCollege.cob
```

Run the currently configured default test suite:
```bash
./run_tests.sh
```

Run a specific test root:
```bash
./run_tests.sh /workspace/Tests/Epic9
```

Test runner behavior:
- discovers test cases by `InCollege-Input.txt` or `INPUT.DAT`
- writes actual outputs into each case as `ACTUAL-InCollege-Output.txt`
- reseeds `.DAT` fixtures per test for isolation
- loads epic-level fixtures such as `Tests/Epic9/USERS.DAT` and `Tests/Epic9/MESSAGES.DAT`
- supports per-test fixture overrides by placing `.DAT` files directly in a test case folder

### Preparing Input For Message Viewing
To test viewing messages from an input file, include a login flow followed by:
1. `8` to open the Messages menu
2. `2` to choose `View My Messages`
3. `3` to return to the post-login menu
4. `5` to log out, if needed by the scenario

The resulting output will be written to:
- the terminal
- `/workspace/InCollege-Output.txt`

### Finished Project Notes
- The executable is built from `src/InCollege.cob`, which includes the copied source modules needed for a single-file build.
- Messaging output includes sender, message body, and timestamp formatting required for Week 9.
- The repository now includes expected output files for the Week 9 truncated-message test cases so they can run as normal regression tests.
