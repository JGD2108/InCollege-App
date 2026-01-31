Tests directory structure and conventions

Overview
- Tests organized by feature/module under `Tests/`.
- Each test case lives in its own folder and should contain at minimum `INPUT.DAT` and `OUTPUT.DAT`.
- Optional second-run files: `INPUT2.DAT` and `OUTPUT2.DAT` for persistence checks.

Naming and structure
- Feature folders: descriptive names (e.g., `NM-7_NewUserRegistration`).
- Test folders: `TC-XXX_Description`.

Metadata: TEST.meta
- Place an optional `TEST.meta` file in a test folder to control runner behavior.
- Supported keys (case-insensitive):
  - `baseline=true|false` — force whether `USERS.DAT` should be seeded from `USERS.DAT.baseline` before the test.

Example TEST.meta
baseline=true

Running the suite
- Default entrypoint: `run_tests.sh` (runs `Tests/Epic1` by default).
- Override test directory by passing it as the first argument: `./run_tests.sh /workspace/Tests/NM-7_NewUserRegistration`

Best practices
- Keep test inputs minimal and deterministic.
- Use `INPUT2.DAT` only when testing state persistence across runs.
- Use `TEST.meta` when the simple heuristic (first non-empty input line = "1") is insufficient.
