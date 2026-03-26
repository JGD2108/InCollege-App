## InCollege (COBOL)

### Overview
InCollege is a menu-driven COBOL application built from `src/InCollege.cob`.

Current implemented flows include:
- account creation and login
- profile creation/editing and viewing
- connection requests and network viewing
- job posting
- job browsing with summary listings
- full job detail viewing
- applying to jobs/internships
- viewing a per-user application report

The compiled executable is `/workspace/bin/InCollege`.

### Prerequisites
- Open the project in the provided dev container, or use an environment with `cobc` installed.

### Build
```bash
mkdir -p bin
cobc -x -free -o bin/InCollege src/InCollege.cob
```

### Run
The program reads scripted input from:
- `InCollege-Input.txt`

It writes the exact same printed output to:
- `InCollege-Output.txt`

Run:
```bash
/workspace/bin/InCollege
```

### Menus
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

Job Search/Internship menu:
- `1. Post a Job/Internship`
- `2. Browse Jobs/Internships`
- `3. View My Job Applications`
- `4. Back to Main Menu`

### Week 7 Job Features
`Browse Jobs/Internships` now:
- lists all saved jobs with title, employer, and location
- lets the user choose a job number to view full details
- allows the user to apply to a selected job

When a user applies:
- the application is stored persistently in `APPLICATIONS.DAT`
- the saved record links the logged-in username to the selected job ID
- the program prints a confirmation line that includes the job title and employer

`View My Job Applications` now:
- reads the saved application records for the logged-in user
- looks up each matching job in `JOBS.DAT`
- displays the job title, employer, and location for each application
- shows a total application count

### Data Files
The program uses line-sequential `.DAT` files in the workspace root.

- `USERS.DAT`
  - username: `PIC X(12)`
  - password: `PIC X(12)`

- `PROFILES.DAT`
  - username plus profile fields such as first name, last name, college, major, grad year, and about-me text

- `CONNECTIONS.DAT`
  - pending connection requests

- `ESTABLISHED.DAT`
  - accepted/established connections

- `EDUCATION.DAT`
  - saved education entries

- `EXPERIENCE.DAT`
  - saved experience entries

- `JOBS.DAT`
  - job title: `PIC X(30)`
  - description: `PIC X(100)`
  - employer: `PIC X(30)`
  - location: `PIC X(30)`
  - salary: `PIC X(20)`
  - posted by username: `PIC X(12)`
  - job ID: `PIC X(6)`

- `APPLICATIONS.DAT`
  - applicant username: `PIC X(12)`
  - applied job ID: `PIC X(6)`

`APPLICATIONS.DAT` is the persistence layer for Week 7 applications. The application report uses those saved job IDs to fetch display information from `JOBS.DAT`.

### I/O Rules
- All user input is read from `InCollege-Input.txt`.
- Every printed line is displayed on screen and also written to `InCollege-Output.txt`.
- The shared print routine for this behavior is in `src/InCollege.cob`.

### Automated Tests
Run all Epic 7 tests:
```bash
bash /workspace/run_tests.sh /workspace/Tests/Epic7
```

Run a specific Week 7 story:
```bash
bash /workspace/run_tests.sh /workspace/Tests/Epic7/NM-139_ApplyToSpecificJob
bash /workspace/run_tests.sh /workspace/Tests/Epic7/NM-140_ViewAppliedJobs
```

Test runner behavior:
- discovers test cases by `InCollege-Input.txt` or `INPUT.DAT`
- reseeds `.DAT` fixtures per test for isolation
- supports epic-level and per-test `.DAT` fixture overrides
- writes actual outputs as `ACTUAL-InCollege-Output.txt` or `ACTUAL2-InCollege-Output.txt`

### Important Files
- `src/InCollege.cob`: main single-file build target and record definitions
- `src/JOBS_SRC.cpy`: job menu, posting flow, and application-report view
- `src/BROWSEJOBS_SRC.cpy`: browse, details, and apply flow
- `run_tests.sh`: automated test runner
- `Tests/Epic7/`: Epic 7 browse/apply/report regression tests

### Sample Run Files
- `/workspace/InCollege-Input.txt`: current sample scripted run
- `/workspace/InCollege-Output.txt`: output from the matching sample input

These files are useful for a quick manual run, while the `Tests/` folders provide broader story-specific coverage.
