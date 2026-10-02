## Developing inside the container
### AUTHOR: Cole Brookson

This project is developed inside a Docker container via VS Code Dev Containers.
This guarantees that everyone working on the project is running the same R
version, the same packages, and the same system libraries.

### Prerequisites

- [Docker Desktop](https://www.docker.com/products/docker-desktop/) (or Docker Engine on Linux)
- [VS Code](https://code.visualstudio.com/)
- The [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

### Getting started

1. Open the project folder in VS Code.

2. You should see a pop-up in the bottom-right corner:
   > *"Folder contains a Dev Container configuration file. Reopen folder to develop in a container."*
   Click **Reopen in Container**.

3. If the pop-up doesn't appear, open the command palette (`Cmd`+`Shift`+`P`)
   and run **Dev Containers: Reopen in Container**.

4. VS Code will build the Docker image (first time only — subsequent opens use
   the cached image) and attach to it. The terminal will now be running inside
   the container.

5. Verify you're inside the container:
   ```bash
   cat /etc/os-release
   ```
   You should see Ubuntu 22.04 (or whichever base image is current).

### Rebuilding the container

Any time you change `DESCRIPTION`, `Dockerfile`, or `sys_deps/sys_deps.sh`,
rebuild via the command palette:

**Dev Containers: Rebuild Container**

Git is installed in the container, so you can commit and push from the
integrated terminal without leaving VS Code.
