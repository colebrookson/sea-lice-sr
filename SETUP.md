# Setting up a new project from this template

This is a minimal checklist for spinning up a new R project repo from
`r-template`. The whole process should take under ten minutes.

---

## 1. Create the new repo

On GitHub, click **Use this template** or manually copy the files into a new repository.

Clone it locally and open the folder in VS Code.

---

## 2. Rename the project

Edit **`DESCRIPTION`** — at minimum, change these fields:

```
Package: myproject          # <- short snake_case name, no spaces
Title: My R Project         # <- human-readable one-liner
Description: ...            # <- one paragraph
Version: 0.0.0.9000         # <- reset or keep
```

Update the author block with your name and email if not already correct:

```
Authors@R:
    person(given = "Cole",
           family = "Brookson",
           role = c("aut", "cre"),
           email = "cole.brookson@yale.edu")
```

---

## 3. Update the container name

Edit **`Makefile`**, first line:

```makefile
IMAGE  ?= myproject   # <- match the Package name in DESCRIPTION, or a Docker Hub slug
```

Edit **`.devcontainer/devcontainer.json`**, `"name"` field:

```json
"name": "myproject"
```

---

## 4. Add your project-specific packages

Open **`DESCRIPTION`** and add any packages you know you'll need to `Imports:`.
Keep the seed packages (`conflicted`, `here`, `dplyr`, etc.) unless you have a
reason to remove them.

If any package requires a system library, add the relevant `apt-get install`
line to **`sys_deps/sys_deps.sh`** (see comments in that file for common
examples).

---

## 5. Build and open the container

In VS Code, open the command palette (`Cmd`+`Shift`+`P`) and run:

**Dev Containers: Reopen in Container**

This will build the Docker image for the first time. It takes a few minutes on
the first build; subsequent opens use the cache.

---

## 6. Verify the environment

Inside the container terminal, check that your packages load cleanly (see section 7.1 on using R with `arf` below):

```r
source("packages.R")
source("global.R")
```

No errors means you're good to go.

---

## 7. Start working

- Put analysis scripts in `R/`
- Never develop directly on `main` — branch for every feature or analysis chunk
- When you add a new package mid-project, follow the protocol in
  `docs/adding-dependencies.md`

### 7.1 Use `arf`

Because this is built to be used with VS Code, I hate the default R terminal, so I replaced it in the dependencies with [`arf`](https://github.com/eitsupi/arf) which has great documentation and doesn't break like `radian` does. Start coding by just writing `arf` in the bash terminal and now you have an open R terminal. 

---

## Files reference

| File / Dir                        | Purpose                                                      |
| --------------------------------- | ------------------------------------------------------------ |
| `DESCRIPTION`                     | R dependency manifest — the only place packages are declared |
| `Dockerfile`                      | Container definition built from `DESCRIPTION`                |
| `Makefile`                        | `make build`, `make shell`, `make run`                       |
| `sys_deps/sys_deps.sh`            | System-level `apt` installs                                  |
| `.devcontainer/devcontainer.json` | VS Code Dev Containers config                                |
| `packages.R`                      | Sources all packages declared in `DESCRIPTION`               |
| `global.R`                        | Project-wide utility functions                               |
| `.Rprofile`                       | Session-level options (`here`, `conflicted`, warnings)       |
| `.gitignore`                      | R artifacts, output dirs, raw data                           |
| `R/`                              | Your analysis scripts go here                                |
| `docs/`                           | Internal SOPs for deps and container workflow                |
