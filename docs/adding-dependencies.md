## Adding dependencies to this project
### AUTHOR: Cole Brookson

`DESCRIPTION` is the single location for noting all all R package dependencies, and `pyproject.toml` is for all Python package dependencies. The container is built from both — so anything not declared in one of these two files is not guaranteed to be present in the reproducible environment.

### Adding an R package

1. **Work on a development branch.** No code development on `main`.

2. **Trial the package in your current session** without committing to it yet:
   ```r
   pak::pkg_install("newpackage")
   ```
   This installs into the running container session only — it does not persist
   after the container is rebuilt.

3. **Once you're happy**, add the package to the `Imports:` field in `DESCRIPTION`:
   ```
   Imports:
       dplyr,
       newpackage
   ```

4. **Rebuild the container** via the VS Code command palette:
   `Dev Containers: Rebuild Container`

5. **Test** that the new package loads cleanly and everything still works.

### Adding a system dependency

If the new R package requires a system library (e.g. `libgdal-dev` for `sf`), add the relevant `apt-get install` line to `sys_deps/sys_deps.sh` before rebuilding. The comments in that file list the most common ones.

### Adding a Python package

`pyproject.toml` declares Python dependencies the same way `DESCRIPTION` does for R — add the package there, rebuild, and the container picks it up.

1. **Work on a development branch.** No code development on `main`.

2. **Trial the package in your current session** without committing to it yet:
   ```bash
   pip install newpackage
   ```
   This installs into the running container session only — it does not persist
   after the container is rebuilt.

3. **Once you're happy**, add the package to the `dependencies` field in
   `pyproject.toml`:
   ```toml
   dependencies = [
       "snakemake==9.14.2",
       "newpackage",
   ]
   ```

4. **Rebuild the container** via the VS Code command palette:
   `Dev Containers: Rebuild Container`

5. **Test** that the new package imports cleanly and everything still works.

#### Using Pixi instead of pip

If you'd rather manage Python dependencies with [Pixi](https://pixi.sh) instead of `pip` directly, you can do so without changing where dependencies are declared. Pixi reads the same `dependencies` field in `pyproject.toml` and treats it as a list of PyPI packages — the `[tool.pixi.workspace]` table already present in the file just tells Pixi which channels and platforms to resolve against. There is no second dependency list to maintain.

To add a package with Pixi instead of editing the file by hand:

```bash
pixi add --pypi newpackage
```

This appends `newpackage` to the same `dependencies` array in `pyproject.toml` that the manual approach above edits directly, so the two workflows stay interchangeable. Either way, the rebuild-and-test steps are the same.

### Adding a system dependency for a Python package

System libraries required by a Python package (e.g. `libgeos-dev` for some geospatial packages) go in `sys_deps/sys_deps.sh` alongside the R system dependencies, since both are installed at the same layer of the Dockerfile.