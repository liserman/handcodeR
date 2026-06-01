# handcodeR Web App — Rebuild Plan & Documentation

Status: **in progress** (branch `beta_version_0.2.1-app-test`). Milestones 2–5
+ 7 done (full app collapsed into two self-contained files `app_local.R` /
`app_docker.R`, all four modes, both delivery paths); only milestone 6
(containerize) remains. Functionally complete; needs browser click-through.
This document is the design spec and living documentation for the Shiny
**configurator app** that wraps the `handcodeR` package and serves it as a hosted
web application (Docker).

It is deliberately separate from [../CLAUDE.md](../CLAUDE.md): that guide governs
**package** code under `R/`. The app under `App/` is explicitly *out of scope* of
the package style rules (it may use `library()`, standalone server code, looser
conventions). Where it does not cost anything, the app still **borrows** the
package's good habits — section banners, WHY-comments, the `.check_/.prepare_/
.build_` naming families, snake_case, aligned named lists — so the two halves
read as one project.

---

## 1. Why this rebuild exists

The original prototype (kept for reference as
[app_prototype.R](app_prototype.R)) is broken against the reworked package. It
calls an old, removed API:

| Prototype calls (gone) | Reworked package equivalent |
|------------------------|-----------------------------|
| `handcodeR:::handcoder_ui(a_obj, mode)` | `.build_categorial_ui(app_data)` (+ `.build_comparison_ui` / `.build_binary_ui` / `.build_binary_comparison_ui`) |
| `handcodeR:::handcoder_server(input, output, session, a_obj, mode)` | `.categorial_server(app_data)` (+ `.comparison_server` / `.binary_server` / `.binary_comparison_server`) — each **returns** a `function(input, output, session)` |
| `handcodeR:::character_to_data(...)` | `.character_to_data(data, arg_list, missing, prefix, comparison)` |
| `handcodeR:::data_for_app(...)` | manual `app_data <- list(...)` bundle built inside the entry point, after `.prepare_data()` |

There is **no `mode` argument anywhere** in the reworked package, and **no web
seam exists yet**. Every `.run_*_app(app_data)` hard-calls
`shiny::runApp(shiny::shinyApp(ui, server))` and *blocks*; both `handcode()` and
`handcode_binary()` `stop()` immediately when `!interactive()`; and the session
returns its result by `shiny::stopApp(annotated)` to the calling R process. None
of that survives a headless container.

The good news: the UI builders and server factories are **already decoupled from
launch**. `.build_categorial_ui(app_data)` returns a UI object and
`.categorial_server(app_data)` returns a server function — `.run_categorial_app`
is just a three-line `runApp(shinyApp(...))` wrapper over them. So a host app can
embed the coder by calling the builder + factory directly; it does **not** need a
new package "web mode."

So the app cannot be patched line-by-line; it must be rebuilt against the new
pipeline. Two things are being designed at once:

1. A small set of **package-side adjustments** (§4) so the coder can run embedded
   and headless — not a new mode, just removing the three hard assumptions that
   only hold for an interactive desktop gadget.
2. The **app itself** — a configurator front-end + the embedded coder — packaged
   to run as a long-lived web service in a container.

---

## 2. Two deployment targets (decided)

The project ships **two optimized versions** over one shared package core:

Delivered as **two self-contained `app.R` files** (final shape, §9):

| | **`app_local.R`** (RStudio) | **`app_docker.R`** (hosted web) |
|---|---|---|
| Audience | single user, from RStudio | many users, hosted (one process each) |
| Flow | sequential configurator → coder, in the viewer pane | sequential configurator → coder, served per user |
| Launch | run from RStudio; app calls `runApp()` and catches the return | container calls `runApp()`; app catches the return |
| Result delivery | **returns annotated frame to the R workspace** (like `handcode()`) | `runApp()` return → **download** (CSV + RDS) |
| Concurrency | one session | one R process per user (**ShinyProxy**), §7 |
| Quicksave | allowed (`save_loc` set) | **off** (`save_loc = NULL`) |

(The package's own `handcode()` / `handcode_binary()` gadgets remain available and
unchanged for users who prefer typing the call directly.)

The shared core is the package's validate → `app_data` → `.build_*_ui` +
`.*_server` pipeline, **completely unchanged**. The two versions differ only in
who calls `runApp()` and what happens to the returned frame.

Both app files use **zero package changes**. They keep `stopApp` fully: the app
launches the package's coder via its *own* `runApp()` and **catches** the frame
`stopApp` returns (`runApp()` returns exactly that value). `app_local.R` hands it
back to the workspace; `app_docker.R` serves it as a download. This forces the
sequential / one-process shape (§3, §7), because `runApp()` blocks and cannot be
nested. The package's standalone `handcode()` gadget stays as-is for direct use.

---

## 3. Target architecture

The package is already a clean pipeline of single-purpose helpers:

```
entry point (validate -> normalize -> build app_data)
  -> .build_*_ui()      (UI tree)
  -> .*_server()        (wires outputs + handlers)
  -> .run_*_app()       (launches, blocks, returns annotated data)
```

The four coding variants (categorial, comparison, binary, binary-comparison) each
have their own `.build_*_ui(app_data)` + `.*_server(app_data)` + a thin
`.run_*_app(app_data)` launcher. The launcher is the *only* part bound to the
blocking-gadget lifecycle; the builder and factory above it are reusable as-is.
The integration seam is therefore "call the builder + factory directly and host
them yourself," not a flag on the launcher.

### 3.1 The app is two sequential phases (per user process)

Because `runApp()` blocks and cannot be nested, the two phases are **separate,
sequential `runApp()` calls** in one R script — not one always-on Shiny session.
This is the gadget pipeline shape, run once per user (ShinyProxy, §7).

```
  R process (one per user)
  ───────────────────────────────────────────────────────────────
  config    <- runApp(configurator_app)   # Phase 1
                 upload CSV -> pick columns -> define variables
                 -> options -> live handcode() preview -> [Launch]
                 -> stopApp(config)
                 │
                 v
  app_data  <- build_app_data(config)      # validate + assemble (app-side)
                 │   save_loc = NULL        # quicksave forced off (web)
                 v
  annotated <- runApp(shinyApp(            # Phase 2: package-owned coder
                 .build_<mode>_ui(app_data),
                 .<mode>_server(app_data)))
                 -> code rows -> Save & Exit -> stopApp(annotated)
                 │   ← app CATCHES this return value
                 v
  Phase 3: write annotated to file / serve as download
```

Phase 1 is the app's own configurator. Phase 2 is **package-owned and unchanged**:
the app calls `.build_<mode>_ui(app_data)` + `.<mode>_server(app_data)` for the
chosen variant inside its own `runApp()`. When the user saves, the package's
`stopApp(annotated)` makes that `runApp()` return the frame — the app catches it.
`app_data` is immutable once built; mutable state lives only in the package's
`reactiveValues` (`.init_server_values`).

The exact `app_data` contract (reproduced from the `handcode()` body — the app
builds this list itself, since no exported assembler exists):

```r
app_data <- list(
  data            = prepared$data,          # from .prepare_data()
  original_data   = prepared$original_data,
  start_val       = prepared$start_val,
  classifications = class_args,             # named list var = c(levels)
  context         = context,                # TRUE / FALSE / "FLEX"
  add_notes       = add_notes,
  missing         = missing,
  enable_numeric  = enable_numeric,
  save_loc        = NULL                     # WEB: always NULL -> no quicksave
)                                            # (R version: .quicksave_setup(quicksave, name))
# comparison adds: app_data$pre_comparison, app_data$post_comparison
# binary adds:     app_data$multifactorial, app_data$enable_numeric, app_data$colors
```

---

## 4. Package-side work — **NONE REQUIRED** (decided)

Owner decision (2026-05-31): the web build uses **zero package changes**. The
three assumptions that looked like blockers are all sidestepped at the app level.
This section records *why* each is a non-issue, so nobody "fixes" them later.

**Gap 1 — `interactive()` guard.** The guard lives in `.check_cat_session` /
`.check_bin_session`, which only run inside the `handcode()` entry points. The
app does **not** call the entry points — it calls `.character_to_data` +
`.prepare_data` + `.build_*_ui` + `.*_server` directly. The guard never fires.
No change. (Cost: the app reproduces the entry point's validate→assemble logic;
acceptable, and §6 keeps validation pointed at the shared `.check_*`.)

**Gap 2 — `stopApp()` result delivery — kept as-is, caught by the app.**
`.setup_save_handler` returns the annotated frame via `shiny::stopApp(annotated)`
(and on `session$onSessionEnded`). The app launches the coder in its **own**
`runApp()`; `runApp()` returns whatever `stopApp()` was given, so the app catches
the frame and serves it as a download. `stopApp` stays the package's behavior. No
change. (Constraint: blocking/non-nestable `runApp` → sequential phases, one
process per user, §3.1 / §7.)

**Gap 3 — quicksave — forced off in web via config, not code.** Quicksave renders
only when `app_data$save_loc` is set. The web app hard-sets `save_loc = NULL`, so
the button never appears and the handler never fires — quicksave is **not
activatable** from the web, regardless of configurator state. The R version keeps
it through the `quicksave` arg. No change.

**No shared launcher.** R-native keeps the `handcode()` / `handcode_binary()`
blocking gadget. The web app owns its own sequential `runApp()` calls (§3.1). The
package stays launch-policy-free; no exported `handcode_app()` is added.

---

## 5. App-side work (under `App/`)

### 5.1 File layout (current)

The dev scaffolding (`App/R/` modules, `app.R`, `run_dev*.R`) has been **inlined
and deleted** (milestone 7 done). The shipped app is exactly two self-contained
files:

| Path | Contents | Status |
|------|----------|--------|
| `App/app_local.R` | RStudio app (self-contained): configurator → coder → **return to workspace**; quicksave on; runs in viewer | done |
| `App/app_docker.R` | hosted web app (self-contained): configurator → coder → **download** (CSV+RDS); 50 MB/50k-row caps; CSV escaping; `save_loc=NULL` | done |
| `App/example.csv` | bundled demo dataset ("Load example data") | done |
| `App/app_prototype.R` | original prototype, porting reference (removable) | reference |
| `App/Dockerfile`, `.dockerignore`, ShinyProxy config | container/runtime | step 6 |

Each file inlines everything: helpers (`%||%`, `HANDCODE_RESERVED`,
`BINARY_DEFAULT_COLORS`, CSV escaping), `build_app_data()` (all four modes), the
plain `configurator_ui`/`configurator_server` (no Shiny modules), and the
sequential orchestration. The bulk is duplicated across the two files by design
(§9). `HANDCODER_DEFS_ONLY=1` is a test hatch: load definitions without launching.

Run:
- **local (RStudio):** `source("App/app_local.R")` → result lands in
  `handcode_result`.
- **docker:** `Rscript App/app_docker.R` (binds `0.0.0.0`, `PORT` env or 3838).

### 5.2 What carries over from the prototype (logic worth keeping)

The prototype's configurator logic is sound; only its package calls are stale.
Port these behaviors:

- CSV upload -> `char_columns` (character cols only feed text/comparison/pre/post
  pickers). [app_prototype.R:180-189](app_prototype.R#L180-L189)
- "Use existing handcodeR dataset?" auto-detection from column names
  (`texts`, `comparison`, `pre/post`, `comparison_pre/post`, `notes`, 1–6 vars).
  [app_prototype.R:194-223](app_prototype.R#L194-L223)
- Dynamic variable rows, add-button, value restore across redraws.
  [app_prototype.R:243-300](app_prototype.R#L243-L300) — but **drop the
  `max_vars = 6` cap** (decided: unlimited, §9).
- `start_value()` resolution: numeric textbox wins, else switch
  `first_empty`/`all_empty`. [app_prototype.R:349-369](app_prototype.R#L349-L369)
- Live `handcode(...)` preview that only prints non-default args.
  [app_prototype.R:373-474](app_prototype.R#L373-L474)
- Optional pre/post and comparison pre/post, gated on `context`.
  [app_prototype.R:305-332](app_prototype.R#L305-L332)

### 5.3 What changes

- Replace `character_to_data()` -> `.character_to_data()` + `.prepare_data()`;
  `data_for_app()` -> the `app_data` list assembly shown in §3.1.
- Replace `handcoder_ui/handcoder_server` mount -> launch `.build_*_ui(app_data)`
  + `.*_server(app_data)` for the chosen variant inside the app's own `runApp()`
  and catch the returned frame (§3.1). No package change needed (§4).
- Add an **explicit mode selector** (radio, decided §9): categorial / comparison
  / binary / binary-comparison. Drives preview, validation, and which
  `.build_*_ui` / `.*_server` launches. The prototype only built `handcode()`.
- Add binary **color pickers** (`colors$left/$right`, hex-validated to match
  `.check_colors_bin`) and the `multifactorial` / `enable_numeric` / `quickcode`
  toggles. These are mutually exclusive in several combinations
  (`.check_binary_params`: quickcode⊕enable_numeric, quickcode needs
  multifactorial, quickcode ≤1 var, enable_numeric ≤9 vars) — reject explicitly,
  do not silently resolve.
- Web-safe **result delivery** (§4 Gap 2): the app catches the frame that
  `runApp()` returns and offers it as a **download** (decided, §9) — never the
  host's home dir; an optional mounted volume (§7) may also receive a copy.
- **Quicksave off**: hard-set `app_data$save_loc = NULL` in the web build so the
  button never renders (§4 Gap 3); do not expose a quicksave control in the
  configurator.

---

## 6. Validation & error surfacing

Reuse the package `.check_*` family as the single source of truth — the app must
**not** duplicate validation rules. Flow: configurator collects raw inputs ->
calls the relevant `.check_*` -> on `stop()`, the app catches and renders the
message inline (the messages are already lowercase, name the arg, give the valid
form). This keeps one rulebook and guarantees the app and `handcode()` reject the
same inputs identically.

Configurator-only guards (before handing off): a text column is selected, at
least one complete variable row (name + levels), CSV parsed, binary mode has
exactly two levels per variable.

---

## 7. Docker / hosting

The **web version's** deployment target is a **long-lived web service** for
(theoretically) unlimited users. The R-native version does not use any of this.

- **Base image:** `rocker/shiny` or `rocker/r-ver` + `shiny`/`shiny-server`.
  `rocker/shiny` gives a supervisor + logging out of the box.
- **Install:** system libs for the dependency tree (`shiny`, `shinyWidgets`,
  `shinyjs`, `bslib`), then the `handcodeR` package itself (from the built
  tarball or `remotes::install_local()`), then copy `App/`.
- **Entrypoint:** a small R script that runs the sequential phases (§3.1):
  `runApp(configurator)` → `build_app_data()` → `runApp(coder)` → catch return →
  download/write. Bind `0.0.0.0`, expose `3838`.
- **Per-user process (decided, §9):** because each phase is a blocking `runApp`
  and the result comes back via the `runApp` return value, the model is **one R
  process per user** — i.e. **ShinyProxy** spins up a container per user. This is
  what makes catching the `stopApp` return value (and keeping `stopApp`
  unchanged) work. A single always-on multi-user Shiny Server process is **not**
  used, since one user's `stopApp` would end the shared process.
- **TLS / routing:** nginx/Traefik in front of ShinyProxy.
- **State:** results leave via download (§9). Optionally mount a writable volume
  (`/data`) as a checkpoint sink — never a user home dir. Quicksave is off (§4).
- **Config via env:** upload size limit (`shiny.maxRequestSize`), default
  mode/colors — read at startup.
- `.dockerignore` excludes `man/`, `tests/`, `.git/`, dev artifacts.

A `docker-compose.yml` (app + optional volume + proxy) is the convenient local
and small-deployment form; ShinyProxy config is the form for the unlimited-user
target.

---

## 8. Build order (milestones)

1. **No package work** (§4) — decided. Start straight on the app.
2. **App skeleton** — ✅ DONE. Configurator modularized into `App/R/` (upload,
   variables, options, preview), live preview, demo data, returns config via
   `stopApp(config)`. Smoke-tested with `testServer` + app-object build.
3. **Handoff** — ✅ DONE (categorial). `build_app_data.R` replicates `handcode()`'s
   validate→assemble into `app_data` (fresh + resume-with-inferred-levels); the
   coder launches in its own `runApp()` and the return is caught; `run_dev.R`
   runs both phases for browser click-through. Smoke-tested: build_app_data
   (fresh+resume), coder app builds, `.categorial_server` inits via `testServer`.
   **Caveat:** fresh coding keeps only `texts` + annotation columns (matches the
   package's char-vector input); other uploaded columns are dropped. Revisit if
   users need metadata columns preserved.
4. **All four modes** — ✅ DONE. `build_app_data` dispatches categorial /
   comparison / binary / binary_comparison; `run_dev.R` switches builder/server
   by mode; configurator has the mode radio + binary opts (multifactorial,
   quickcode, enable_numeric, left/right colors with package defaults
   #10b981/#dc2626). All `.check_*` reused. Smoke-tested: all four build +
   coder app builds + server inits; binary rejects a 3-level variable.
5. **Web result delivery** — ✅ DONE. `deliver.R` builds a Phase-3 `download_app`
   (CSV + RDS buttons, preview, CSV formula-injection escaping §11); `run_dev_web.R`
   chains configurator → coder → download and sets the 50 MB upload cap. Local
   flow (`run_dev.R`) returns the frame to the workspace. Smoke-tested: escaping,
   CSV/RDS roundtrip, app builds.
6. **Containerize** (§7): Dockerfile, ShinyProxy config, env config.
7. **Collapse to two self-contained files** — ✅ DONE (pulled ahead of step 6).
   `app_local.R` (RStudio: viewer, return to workspace, quicksave on) and
   `app_docker.R` (download, `save_loc = NULL`, 50 MB/50k caps, CSV escaping);
   `App/R/`, dev `app.R`, `run_dev*.R` deleted. Smoke-tested: both load
   self-contained, build all four modes, configurator + coder + download build.

---

## 9. Decisions (owner-confirmed 2026-05-31)

- **Two optimized versions** over one shared core. *R-native:* existing
  `handcode()` / `handcode_binary()` gadget, single user, `stopApp` return —
  unchanged. *Web:* sequential configurator → coder, served per user.
- **Zero package changes.** `stopApp` is kept. The web app launches the coder in
  its own `runApp()` and **catches the returned frame** (`runApp()` returns the
  `stopApp` value). No `on_save` parameter, no exported launcher needed.
- **Web result delivery: download button.** Optional volume copy as a checkpoint
  only; not the primary path.
- **Quicksave: R-only.** Web build hard-sets `app_data$save_loc = NULL` → button
  never renders, handler never fires. Not activatable from the web.
- **Concurrency: R-native = one user; web = many** → **ShinyProxy, one R process
  per user** (§7). Required by the catch-the-`runApp`-return approach; a single
  shared multi-user process is ruled out (one `stopApp` would kill all).

- **Download format: both** — CSV (universal) + RDS (preserves factors/types,
  same shape the R version returns). Two buttons.
- **Modes: all four from the start** — categorial, comparison, binary,
  binary-comparison. No phased mode rollout.
- **Resume: yes** — the web app accepts a previously-exported handcodeR dataset
  and continues coding it. Configurator auto-detects handcodeR-shaped CSVs (the
  prototype's `texts`/`comparison`/`pre`/`post`/`notes` + 1–6 var detection,
  §5.2) and feeds the data-frame resume path of the package.
- **CSV import: fixed UTF-8 + comma** — `read.csv(..., stringsAsFactors = FALSE)`
  like the prototype. No separator/encoding pickers; non-comma files are the
  user's job to convert.
- **UI language: English** — match the package (buttons, citation, messages stay
  English). No German strings, no language switch.
- **Variable count: unlimited** — drop the prototype's `max_vars = 6` cap.
  Caveat: keyboard/`enable_numeric` shortcuts only address 1–9, so the
  configurator should warn (not block) when >9 vars are combined with
  `enable_numeric`, mirroring `.check_cat_numeric_param` / `.check_binary_params`.
- **Mode selection: explicit** — a radio in the configurator picks one of
  categorial / comparison / binary / binary-comparison. No auto-derivation; the
  chosen mode drives which `.build_*_ui` / `.*_server` is launched and which
  `.check_*` runs.
- **Keep the live `handcode()` preview** — show the equivalent `handcode(...)` /
  `handcode_binary(...)` call so users can reproduce the run in R.
- **Public, no login — but hardened.** Anyone with the URL can use it; therefore
  security is a first-class requirement. See §11.
- **Bundle a demo CSV** — a "Load example data" button so users can try the app
  without uploading. Ship a small `App/example.csv`.
- **Branch: `beta_version_0.2.1-app-test`** — build on a new branch off
  `beta_version_0.2.1`, merge later.
- **Export content: package standard** — exactly what `handcode()` returns
  (original columns + annotation variables + `notes` if enabled), in both CSV and
  RDS. `.gen_output` already produces this; the app just serializes it.
- **Final shape: exactly two self-contained `app.R` files, no shared R code.**
  `app_docker.R` (hosted web) and `app_local.R` (RStudio). Each inlines
  everything (helpers, configurator, build_app_data, coder embedding); only
  `example.csv` is shared data. Some logic is duplicated across the two — accepted.
  The current `App/R/` modules are **development scaffolding** to be inlined into
  the two files and then deleted (final milestone).
- **`app_local.R` is optimized for RStudio use**: launched from RStudio, runs in
  the viewer pane, **returns the annotated frame to the R workspace** (like
  `handcode()`), quicksave allowed. No download needed. (vs `app_docker.R`:
  download delivery, `save_loc = NULL`, hardened, ShinyProxy.)
- **XSS: leave package as-is + strict CSP header at proxy** (§11). Self-XSS only
  under per-user isolation; keeps zero package changes.
- **Upload limits: generous** — 50 MB max file, 50,000 rows max (§11).

Still open (lower priority):
- **Internal helper access:** keep `:::` to the package internals
  (`.character_to_data`, `.prepare_data`, `.build_*_ui`, `.*_server`) the app
  depends on, or promote them to a documented contract. Currently: keep `:::`,
  coupling recorded in §10.

---

## 10. Quick reference — reworked package surface (verified against R/handcode.R)

Exported: `handcode(data, ..., start, randomize, context, missing, pre, post,
add_notes, enable_numeric, quicksave)` and `handcode_binary(data, ..., start,
randomize, ...)`. **No `mode` argument exists. Both `stop()` when not
interactive.**

Validation (`.check_*`): `.check_common_params`, `.check_data_first_col`,
`.check_cat_session`, `.check_cat_args`, `.check_cat_numeric_param`,
`.check_comparison_args`, `.check_comparison_col`, `.check_comparison_context`,
`.check_bin_session`, `.check_binary_args`, `.check_binary_params`,
`.check_colors_bin`. *(No `.check_colors_quick`, no `.check_classification_args`,
no `.check_general_args` — those names in earlier notes were wrong.)*

Data prep / IO: `.prepare_data(data, start, randomize, context, pre, post,
extra_exclude)`, `.character_to_data(data, arg_list, missing, prefix,
comparison)`, `.relevel_data_factors`, `.init_comparison_context`,
`.cleanup_comparison_columns`, `.gen_output`, `.quicksave_setup`, `.format_NA`,
`.sanitize_id`, `.get_current_value`.

State / handlers: `.init_server_values(app_data)`, `.init_annotations`,
`.setup_common_outputs`, `.setup_nav_handler`, `.setup_quicksave_handler`,
`.setup_save_handler` (← **returns frame via `stopApp`; app catches it, §4**), `.setup_categorial_panels`,
`.setup_binary_panels`, `.setup_comparison_outputs`, `.make_categorial_handler`,
`.make_binary_handler`.

UI / styles: `.build_app_shell`, `.build_categorial_ui`, `.build_comparison_ui`,
`.build_binary_ui`, `.build_binary_comparison_ui`, `.build_cat_keyboard_script`,
`.build_binary_keyboard_script`, `.common_styles`, `.binary_styles`,
`.comparison_styles`, `.darken_hex`, `.lighten_hex`.

Servers / launch (the seam): `.categorial_server(app_data)`,
`.comparison_server`, `.binary_server`, `.binary_comparison_server` — each
returns `function(input, output, session)`. `.run_categorial_app(app_data)` (+
siblings) = `shiny::runApp(shiny::shinyApp(.build_*_ui(app_data),
.*_server(app_data)))`. **Take the builder + factory, skip `.run_*`.**

Deps (DESCRIPTION): shiny ≥1.7, shinyWidgets ≥0.7.6, bslib ≥0.5.0, shinyjs ≥2.1.
Package version 0.2.1.

---

## 11. Security — public, untrusted-user hardening (decided: public, no login)

The app is **public**: anyone can upload a file and run a session. Treat every
upload as hostile. Threats and mitigations, grouped.

**Per-user isolation (the foundation).** ShinyProxy gives each user their **own
container** (§7). One user cannot see, reach, or crash another's session, and a
compromised session is confined to a throwaway container. This single fact
neutralizes most multi-tenant risk — keep it non-negotiable.

**Rendered text = XSS surface (must address).** The package renders the text to
be coded with `shiny::HTML(values$data$texts[...])` (and `before`/`after`) —
i.e. **raw uploaded content as HTML**. A malicious CSV can carry `<script>`.
Because each user only ever sees their *own* upload in their *own* container,
this is **self-XSS** (the attacker can only script their own session) — impact
low. **Decision (§9): leave the package as-is and add a strict
`Content-Security-Policy` header at the proxy** as the safety net. No package
change, no app-side text mangling (legitimate HTML in texts still renders). If
per-user isolation is ever dropped, revisit — it would become real stored-XSS.

**Resource exhaustion / DoS.** Public upload + R in a container invites
oversized inputs.
- Cap upload size: `options(shiny.maxRequestSize = 50*1024^2)` (**50 MB**,
  decided §9) at startup.
- Cap row count after parse (configurator guard): reject **> 50,000 rows**
  (decided §9) with a clear message before building `app_data`.
- ShinyProxy: per-container **CPU and memory limits**, **max concurrent
  containers**, and an **idle/session timeout** so abandoned sessions are reaped.
- Rate-limit at the proxy (nginx/Traefik) to blunt container-spawn floods.

**Container hardening.**
- Run as **non-root**; **read-only root filesystem** with only a small writable
  tmp; `--cap-drop ALL`; `no-new-privileges`.
- **No outbound network** from the coding container (nothing in the app needs
  egress) — blocks data exfiltration if a session is abused.
- Minimal base image; rebuild for CVE patches.

**CSV formula injection (downstream).** Exported CSV opened in Excel can execute
cells beginning with `= + - @`. Since we offer CSV download, **prefix-escape**
such cells on export (or document the risk). RDS is unaffected.

**Transport & secrets.** TLS terminated at the proxy. No secrets in the image;
config via env. Logs must not contain uploaded text.

**Decision note:** the *application* hardening (upload caps, row caps, container
limits, CSV-export escaping, CSP) needs **no package change**. The *only* item
that would touch the package is escaping the HTML-rendered text; flagged above as
optional and deferred.
